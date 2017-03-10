{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Control.Exception(Exception, IOException, throwIO, try)
import Control.Monad(when)
import Data.IORef(newIORef, readIORef, writeIORef)
import Data.Ratio(denominator, numerator, (%))
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Typeable(Typeable)
import Data.Word(Word64)
import Foreign.Marshal.Alloc(allocaBytes)
import Numeric(showFFloat)
import Options(Option(optionDescription, optionLongFlags, optionShortFlags),
               Options(..), defineOption, optionType, runCommand)
import System.Exit(ExitCode(ExitFailure), exitWith)
import System.IO(Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek, SeekFromEnd), hFlush,
                 hFileSize, hGetBuf, hSeek, hTell, stdout, withBinaryFile)
import System.Random.TF(TFGen, newTFGen)
import System.Random.TF.Instances(randomR)

default_prob = 1/16
default_blocksize = 1048576

description = "test-read a random sample of data in files"

data Param = Param Rational Int

instance Options Param where
    defineOptions = Param <$>
        defineOption optionType_prob
          (\o -> o{optionShortFlags = "p",
                   optionLongFlags = ["prob"],
                   optionDescription = "sampling probability"})
        <*>
        defineOption optionType_blocksize
          (\o -> o{optionShortFlags = "b",
                   optionLongFlags = ["bs"],
                   optionDescription = "block size per sample, # of bytes"})

show_prob r = show (numerator r) ++ "/" ++ show (denominator r)

optionType_prob = optionType "fraction, 0 <= M/N <= 1" default_prob read_prob show_prob
read_prob s = case comp of
    [(r, "")] | 0 <= r && r <= 1 -> Right r
              | otherwise -> Left "must be between 0 and 1 inclusive"
    _ -> Left "must be a fraction M/N"
  where
    comp = [(m%n, s2) | (m, '/':s1) <- reads s, (n, s2) <- reads s1]

optionType_blocksize = optionType "positive integer" default_blocksize read_blocksize show
read_blocksize s = case reads s of
    [(n, "")] | n >= 1 -> Right n
              | otherwise -> Left "must be at least 1"
    _ -> Left "must be a positive integer"

main = runCommand $ \(Param prob blocksize) files -> do
    infos <- mapM getfileinfo files
    let total = fromIntegral (sum (map fi_size infos))
        total :: Double
    r <- read_all prob blocksize infos total
    case r of
        Pass -> putStrLn "passes test"
        Error ioe loc -> do
            print ioe
            putStrLn ("error location: " ++
                      showFFloat (Just 0) (fromIntegral loc / total * 100) "%")
            exitWith (ExitFailure 1)

data FileInfo = FileInfo {fi_name :: String, fi_size :: !Word64}

getfileinfo filename = withBinaryFile filename ReadMode $ \h -> do
    size <- hFileSize h
            `except` \_ -> do hSeek h SeekFromEnd 0
                              hTell h
    return (FileInfo filename (fromInteger size))

-- dots_full is how many dots represent 100% in the progress bar
dots_full = 50

read_all prob blocksize infos total = do
    dots <- newIORef 0
    let go [] _ = return Pass
        go (FileInfo filename len : more) sumlen = do
            ls <- random_locations blocksize len prob `fmap` newTFGen
            r <- withBinaryFile filename ReadMode
                 (\h -> try_read h blocksize ls (print_dot sumlen))
            case r of
                Pass -> go more $! sumlen+len
                Error ioe loc -> return (Error ioe (sumlen+loc))
        -- progress dots are printed this way: n dots means the most recent
        -- location read is n/dots_full*100% into the files
        print_dot sumlen loc = do
            dots_old <- readIORef dots
            let dots_new = ceiling (fromIntegral (sumlen+loc) / total * fromIntegral dots_full)
                diff = dots_new - dots_old
            when (diff > 0) $ do
                putStr (replicate diff '>')
                hFlush stdout
                writeIORef dots dots_new
    putStr (replicate (dots_full - 1) '-') >> putStrLn "|"
    r <- go infos 0
    putStrLn ""
    return r

{- Assume 1 <= /step/ <= /len/, 0 <= /prob/ <= 1.

Let /S/ be the set of the multiples of /step/ in 0 to /len/-1 inclusive.

@random_locations@ /step len prob rg/ uses random generator /rg/ to generate
floor(prob * |/S/|) random numbers in /S/.

/rg/ is the last parameter so that you can write like

> locs <- random_locations step len prob `fmap` newStdGen
-}
random_locations :: Int -> Word64 -> Rational -> TFGen -> Set Word64
random_locations step len prob g = make g k Set.empty
  where
    step_, n :: Word64
    step_ = fromIntegral step
    -- n = |S| = ceiling(len / step)
    n = (len + step_ - 1) `quot` step_
    k :: Int  -- I use Int because Set size is Int anyway
    k = floor (prob * fromIntegral n)
    make _ 0 s = s
    make g k s = case randomR (0, n-1) g of
        (r, g1) | mr `Set.member` s -> make g1 k s
                | otherwise -> make g1 (k-1) $! (Set.insert mr s)
          where mr = r * step_

{- @try_read@ /handle size set notify/ tries to read from /handle/ the locations in /set/
in increasing order, at each location read /size/ bytes. After reading at location
/loc/ say, call /notify loc/. Return @Pass@ or the first I/O error and its location. -}
try_read :: Handle -> Int -> Set Word64 -> (Word64 -> IO a) -> IO Report
try_read handle size set notify = allocaBytes size $ \buf -> do
    e <- try (mapM_ (seekread buf) (Set.toAscList set))
    case e of
        Left (Errloc ioe loc) -> return (Error ioe loc)
        Right _ -> return Pass
  where
    seekread buf loc =
        do hSeek handle AbsoluteSeek (toInteger loc)
           hGetBuf handle buf size
           notify loc
        `except` (\ioe -> throwIO (Errloc ioe loc))
    -- if loc is too large, there is no error, hSeek succeeds, hGetBuf
    -- returns 0. I'm fine with it.

data Report = Pass | Error IOException !Word64

data Errloc = Errloc IOException !Word64 deriving (Show, Typeable)
instance Exception Errloc

except :: IO a -> (IOException -> IO a) -> IO a
except action handler = do
    e <- try action
    case e of
        Left ex -> handler ex
        Right a -> return a

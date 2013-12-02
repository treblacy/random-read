{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Control.Exception(Exception, IOException, throwIO, try)
import Data.Ratio(denominator, numerator, (%))
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Typeable(Typeable)
import Data.Word(Word64)
import Foreign.Marshal.Alloc(allocaBytes)
import Numeric(showFFloat)
import Options.Applicative(arguments1, eitherReader, execParser, help, helper, info,
                           long, metavar, nullOption, progDesc, short, showDefaultWith,
                           value, idm, (<>))
import System.Exit(ExitCode(ExitFailure), exitWith)
import System.IO(Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek, SeekFromEnd),
                 hFileSize, hGetBuf, hSeek, hTell, withBinaryFile)
import System.Random(RandomGen, newStdGen, randomR)

default_prob = 1/20
default_blocksize = 1048576

describe = progDesc "test-read a random sample of data in files"

argparser = Param
            <$> nullOption (long "prob" <> short 'p' <> metavar "M/N"
                            <> help "probability"
                            <> value default_prob
                            <> showDefaultWith show_prob
                            <> eitherReader read_prob)
            <*> nullOption (long "bs" <> short 'b' <> metavar "BYTES"
                            <> help ("block size per read, in bytes")
                            <> value default_blocksize
                            <> showDefaultWith show
                            <> eitherReader read_blocksize)
            <*> arguments1 Just (metavar "FILES" <> help "files to be tested")

data Param = Param Rational Int [String]

show_prob r = show (numerator r) ++ "/" ++ show (denominator r)

read_prob s = case comp of
    [(r, "")] | 0 <= r && r <= 1 -> Right r
              | otherwise -> Left "must be between 0 and 1 inclusive"
    _ -> Left "must be a fraction M/N"
  where
    comp = [(m%n, s2) | (m, '/':s1) <- reads s, (n, s2) <- reads s1]

read_blocksize s = case reads s of
    [(n, "")] | n >= 1 -> Right n
              | otherwise -> Left "must be at least 1"
    _ -> Left "must be a positive integer"

main = do
    Param prob blocksize files <- execParser (info (helper <*> argparser) describe)
    infos <- mapM getfileinfo files
    let total = fromIntegral (sum (map fi_size infos))
        total :: Double
    r <- read_all prob blocksize infos
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

read_all prob blocksize infos = go infos 0
  where
    go [] _ = return Pass
    go (FileInfo filename len : more) sumlen = do
        r <- withBinaryFile filename ReadMode $ \h -> do
            ls <- random_locations blocksize len prob `fmap` newStdGen
            try_read h blocksize ls
        case r of
            Pass -> go more $! sumlen+len
            Error ioe loc -> return (Error ioe (sumlen+loc))

{- Assume 1 <= /step/ <= /len/, 0 <= /prob/ <= 1.

Let /S/ be the set of the multiples of /step/ in 0 to /len/-1 inclusive.

@random_locations@ /step len prob rg/ uses random generator /rg/ to generate
floor(prob * |/S/|) random numbers in /S/.

/rg/ is the last parameter so that you can write like

> locs <- random_locations step len prob `fmap` newStdGen
-}
random_locations :: RandomGen g => Int -> Word64 -> Rational -> g -> Set Word64
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

{- @try_read handle size set@ tries to read from @handle@ the locations in @set@
in increasing order, at each location read @size@ bytes. Returns Pass or the first 
I/O error and its location. -}
try_read :: Handle -> Int -> Set Word64 -> IO Report
try_read handle size set = allocaBytes size $ \buf -> do
    e <- try (mapM_ (seekread buf) (Set.toAscList set))
    case e of
        Left (Errloc ioe loc) -> return (Error ioe loc)
        Right _ -> return Pass
  where
    seekread buf loc =
        do hSeek handle AbsoluteSeek (toInteger loc)
           hGetBuf handle buf size
           return ()
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

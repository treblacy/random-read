Test-read a random sample of the data in the files specified on the command line.
Report the first sampled error location as a percentage of the sum of file sizes.
Example of what I mean (in fact original motivation):

    $ random-read VTS_01_1.VOB VTS_01_2.VOB VTS_01_3.VOB
    VTS_01_2.VOB: hGetBuf: hardware fault (Input/output error)
    error location: 42%

If those files, in that order, hold a DVD movie of 120 minutes, then 42% means
that there is a reading error near 42% × 120 = 50 minutes into the movie.
(It is not 42% into VTS_01_2.VOB; it is 42% into the concatenation of the files.)

This is also supported:

    $ random-read /dev/sr0

Options:

* -h, --help: show help text
* -p, --prob: probability in M/N syntax
* -b, --bs: block size per read, in bytes

See default values in help text or source code.

Example:

    $ random-read --prob=3/1000 --bs=2048 VTS_01_1.VOB VTS_01_2.VOB VTS_01_3.VOB

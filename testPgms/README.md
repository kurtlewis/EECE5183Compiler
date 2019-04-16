# testPgms Directory
This directory is home to test programs which can be used to test the
health of the compiler. 

To run the tests in an automated fashion, run `make test` from the project's
root directory.

# Directory Structure
## correct directory
The correct directory holds entire programs that are correct. These programs
should be free of output other than generated code.

## incorrect directory
`incorrect` is kind of a misnomer for this directory, this is more like a 
unit testing directory. The programs here are divided by what they are testing,
for instance `typing` or `parsing.`. There are two types of files in these
directories, `*.src` files, which are code source, and `*.out`, which is
verified output from the compiler that can be used as a baseline for testing.

Not every line of the `*.src` files in the incorrect programs is 'incorrect',
but they also don't form entire real programs, they are more just setups for
testing specific functionality.

#!/bin/bash
# Calls the kjlc compiler and creates an executable
out/kjlc "$@"
gcc out.s out/runtime.o -lm -o a.out
rm out.s

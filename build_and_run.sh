#!/bin/sh
echo "----------------------"
echo "Run and build: $1"

# use fast for best speed (but slower compilation)
OPT=${2:-0}

rm where_exe
clang -O$OPT -o where_exe $1.c && ./where_exe

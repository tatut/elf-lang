#!/bin/sh

clang -DTEST=1 -o test $1.c
time ./test

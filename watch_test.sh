#!/bin/sh

fswatch -o $1.c | xargs -n1 -I{} ./test.sh $1

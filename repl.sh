#!/bin/sh
cd "$(dirname "$0")"
swipl -q -t repl elf.pl

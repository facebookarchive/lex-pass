#!/bin/sh
# wrapper for iteration while developing

set -e

./install 2>&1 | less -E

# for profiling
#mkdir -p obj
#ghc -O2 --make src/Main.hs -isrc -odir obj -hidir obj -auto-all -caf-all -fforce-recomp

lex-pass "$@"

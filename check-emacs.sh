#!/bin/bash
set -e
#######################################

nix-build

./result/bin/emacs -q --load init.el "$@"

# e.g.
# ./check-emacs.sh ~/haskell/spiros/sources/Data/Digit.hs 

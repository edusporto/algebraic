#!/bin/sh

stack build --exec algebraic-profile --profile && .stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/algebraic-profile/algebraic-profile +RTS -p -RTS && profiteur algebraic-profile.prof

# A Sudoku puzzle solver

An implementation based on logic.

Work in progress.

## Test run

Assuming debian/buster as the programming environment,

    apt-get install cabal-install ghc libghc-hspec-dev
    cabal install permutation

then to run some tests

    cabal run sudoku
    cabal test

# Testing

Travis-ci: [![Build status](https://travis-ci.org/jjhoo/hs-sudoku.svg?branch=master)](https://travis-ci.org/jjhoo/hs-sudoku)

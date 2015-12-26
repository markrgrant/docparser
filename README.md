[![Build Status](https://travis-ci.org/markrgrant/docparser.svg)](https://travis-ci.org/markrgrant/docparser)

# README

A simple text document parser library using parsec.

## Installation

Set up a cabal sandbox

```
cabal sandbox init
cabal install --enable-tests
```


## Tests

Run tests using ``cabal test``


## Coverage

View coverage using

```
hpc report test-docparser --exclude=Main --exclude=runTests
```

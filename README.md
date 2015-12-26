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
hpt report test-docparser --exclude=Main --exclude=runTests
```

A CSS redundancy analyzer that analyzes redundancy.

## Getting Up and Running ##

This will get your dependencies sorted out locally.

    > cabal install cabal-dev cabal-meta
    > cabal-meta --dev install --enable-tests

Then you can run tests one of two ways.

    > cabal-dev test       # compiles source and runs tests
    > runghc test/Suite.hs # runs in interpreted mode


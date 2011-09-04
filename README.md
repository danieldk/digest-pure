# digest-pure

## Introduction

This is a pure Haskell implementation of the
[digest](http://hackage.haskell.org/package/digest) package.

## Unit test

 * For random *ByteStrings*: does the output of *digest-pure* match that of
   *digest*?

## Benchmarks

Benchmarked on a Mac Mini 2010, 2.4GHz, using lazy *ByteString*s, best out
of five.

### adler32

    $ time ./adler32 ~/Downloads/os_x_lion_10.7.2_update_11c40.dmg 
    165386662

    real  0m0.447s
    user  0m0.150s
    sys   0m0.295s

### adler32-pure

    $ time ./adler32-pure data
    165386662

    real  0m2.224s
    user  0m1.924s
    sys   0m0.298s


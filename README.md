
![Build Status](https://github.com/Quid2/flat/actions/workflows/haskell-ci.yml/badge.svg)

[![Hackage version](https://img.shields.io/hackage/v/flat.svg)](http://hackage.haskell.org/package/flat)

[![Stackage LTS 16](http://stackage.org/package/flat/badge/lts-16)](http://stackage.org/lts/package/flat)
[![Stackage LTS 18](http://stackage.org/package/flat/badge/lts-18)](http://stackage.org/lts/package/flat)
[![Stackage LTS 19](http://stackage.org/package/flat/badge/lts-19)](http://stackage.org/lts/package/flat)
[![Stackage Nightly](http://stackage.org/package/flat/badge/nightly)](http://stackage.org/nightly/package/flat)

Haskell implementation of [Flat](http://quid2.org/docs/Flat.pdf), a principled, portable and compact binary data format ([specs](http://quid2.org)).


### How To Use It For Fun and Profit

```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
```

```haskell
import Flat
```



```haskell
data Direction = North | South | Center | East | West deriving (Show,Generic,Flat)
```

Use **flat** to encode: 

```haskell
flat [North,South]
-> "\149"
```


and **unflat** to decode:

```haskell
unflat (flat [North,South]) :: Decoded [Direction]
-> Right [ North , South ]
```


And thanks to Flat's bit-encoding, this list fits in 1 byte (rather than the 5 bytes that would be required by a traditional byte encoding):

```haskell
flatBits [North,South]
-> "10010101"
```



### Performance

For some hard data, see this [comparison of the major haskell serialisation libraries](https://github.com/haskell-perf/serialization).

Briefly:
 * Size: `flat` produces significantly smaller binaries than all other libraries (3/4 times usually)
 * Serialization time: `store`, `persist` and `flat` are faster
 * Deserialization time: `store`, `flat`, `persist` and `cereal` are faster
 * Transfer time (serialisation time + transport time on the network + deserialisation at the receiving end): `flat` is usually faster for all but the highest network speeds

### Documentation

* [Tutorial](http://hackage.haskell.org/package/flat/docs/Flat-Tutorial.html)

* [Hackage Package and Docs](http://hackage.haskell.org/package/flat)

* [Flat Format Specification](http://quid2.org/docs/Flat.pdf)

### Installation

Get the latest stable version from [hackage](https://hackage.haskell.org/package/flat).

### Compatibility

Tested with:

* [GHC](https://www.haskell.org/ghc/) 7.10.3 to 9.4.3 (x64)


* [GHCJS](https://github.com/ghcjs/ghcjs) version 8.6.0.1 (GHC 8.6.2) 



### Known Bugs and Infelicities

* Data types with more than 512 constructors are currently unsupported (but support could be easily added if necessary)

* Longish compilation times

  * To improve performance, `flat` relies on extensive inlining. This unfortunately leads to longer compilation times.

    If you have many data types or very large ones, you might want to: 

      * During development, turn optimisations off (`stack --fast` or `-O0` in the cabal file).

      * Keep your serialisation code in separate modules.

* See also the [full list of open issues](https://github.com/Quid2/flat/issues).

### Ports for other languages

[Rust](https://www.rust-lang.org/) and [TypeScript-JavaScript](https://github.com/Quid2/ts) ports are under development.


Get in touch if you would like to help porting `flat` to other languages.

### Acknowledgements

`flat` reuses ideas and readapts code from various packages, mainly: `store`, `binary-bits` and `binary` and includes bug fixes from a number of contributors.

### Other Stuff You Might Like

To decode `flat` encoded data you need to know the type of the serialised data.

This is ok for applications that do not require long-term storage and that do not operate in open distributed systems.

For those who do, you might want to supplement `flat` with [ZM - Language independent, reproducible, absolute types](https://github.com/Quid2/zm).

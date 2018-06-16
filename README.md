
[![Build Status](https://travis-ci.org/Quid2/flat.svg?branch=master)](https://travis-ci.org/Quid2/flat)
[![Hackage version](https://img.shields.io/hackage/v/flat.svg)](http://hackage.haskell.org/package/flat)
[![Stackage Nightly](http://stackage.org/package/flat/badge/nightly)](http://stackage.org/nightly/package/flat)
[![Stackage LTS](http://stackage.org/package/flat/badge/lts)](http://stackage.org/lts/package/flat)

Haskell implementation of [Flat](http://quid2.org/docs/Flat.pdf), a principled, portable and efficient binary data format ([specs](http://quid2.org)).

### How To Use It For Fun and Profit

To (de)serialise a data type, make it an instance of the `Flat` class.

There is `Generics` based support to automatically derive instances of additional types.

Let's see some code, we need a couple of extensions:

```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
```

Import the Flat library:

```haskell
import Data.Flat
```

Define a couple of custom data types, deriving Generic and Flat:

```haskell
data Direction = North | South | Center | East | West deriving (Show,Generic,Flat)
```

```haskell
data List a = Nil | Cons a (List a) deriving (Show,Generic,Flat)
```

For encoding, use `flat`, for decoding, use `unflat`:

```haskell
unflat . flat $ Cons North (Cons South Nil) :: Decoded (List Direction)
-> Right (Cons North (Cons South Nil))
```


For the decoding to work correctly, you will naturally need to know the type of the serialised data. This is ok for applications that do not require long-term storage and that do not need to communicate across independently evolving agents. For those who do, you will need to supplement `flat` with something like [zm](https://github.com/Quid2/zm).

#### Define Instances for Abstract/Primitive types

 A set of primitives are available to define `Flat` instances for abstract or primitive types.

 Instances for some common, primitive or abstract data types (Bool,Words,Int,String,Text,ByteStrings,Tuples, Lists, Sequences, Maps ..) are already defined in [Data.Flat.Instances](https://github.com/Quid2/flat/blob/master/src/Data/Flat/Instances.hs).

#### Optimal Bit-Encoding

A pecularity of Flat is that it uses an optimal bit-encoding rather than the usual byte-oriented one.

 To see this, let's define a pretty printing function: `bits` encodes a value as a sequence of bits, `prettyShow` displays it nicely:

```haskell
p :: Flat a => a -> String
p = prettyShow . bits
```

Now some encodings:

```haskell
p West
-> "111"
```


```haskell
p (Nil::List Direction)
-> "0"
```


```haskell
aList = Cons North (Cons South (Cons Center (Cons East (Cons West Nil))))
p aList
-> "10010111 01110111 10"
```


As you can see, `aList` fits in less than 3 bytes rather than 11 as would be the case with other Haskell byte oriented serialisation packages like `binary` or `store`.

For the serialisation to work with byte-oriented devices or storage, we need to add some padding:

```haskell
f :: Flat a => a -> String
f = prettyShow . paddedBits
```

```haskell
f West
-> "11100001"
```


```haskell
f (Nil::List Direction)
-> "00000001"
```


```haskell
f $ Cons North (Cons South (Cons Center (Cons East (Cons West Nil))))
-> "10010111 01110111 10000001"
```


The padding is a sequence of 0s terminated by a 1 running till the next byte boundary (if we are already at a byte boundary it will add an additional byte of value 1, that's unfortunate but there is a good reason for this, check the [specs](http://quid2.org/docs/Flat.pdf)).

Byte-padding is automatically added by the function `flat` and removed by `unflat`.

### Performance

For some hard data, see this [comparison of the major haskell serialisation libraries](https://github.com/haskell-perf/serialization).

Briefly:
 * Size: `flat` produces significantly smaller binaries than all other libraries (3/4 times usually)
 * Encoding: `store` and `flat` are usually faster
 * Decoding: `store`, `cereal` and `flat` are usually faster

 One thing that is not shown by the benchmarks is that, if the serialized data is to be transferred over a network, the total transfer time (encoding time + transmission time + decoding time) is usually dominated by the transmission time and that's where the smaller binaries produced by flat give it a significant advantage.

 Consider for example the Cars dataset. As you can see in the following comparison with `store`, the overall top performer for encoding/decoding speed, the transfer time is actually significantly lower for `flat` for all except the highest transmission speeds (about 4 times faster at typical ADSL speeds, 2 times faster at 4G-LTE mobile speeds).

||Store|Flat|
|---|---|---|
|Encoding (mSec)|  3.1|  7.0|
|Decoding (mSec)| 22.6| 30.0|
|Size (bytes)|702728|114841|
|Transmission (mSec) @ 1 MegaByte/Sec|702.7|114.8|
|Transmission (mSec) @ 10 MegaByte/Sec| 70.3| 11.5|
|Transmission (mSec) @ 100 MegaByte/Sec|  7.0|  1.1|
|Total Transfer (mSec) @ 1 MegaByte/Sec|728.4|151.8|
|Total Transfer (mSec) @ 10 MegaByte/Sec| 96.0| 48.5|
|Total Transfer (mSec) @ 100 MegaByte/Sec| 32.7| 38.1|


### Haskell Compatibility

Tested with:
  * [ghc](https://www.haskell.org/ghc/) 7.10.3, 8.0.2, 8.2.2, 8.4.2 and 8.4.3 (x64)
  * [ghc](https://www.haskell.org/ghc/) 7.10.3/LLVM 3.5.2 (Arm7)
  * [ghcjs](https://github.com/ghcjs/ghcjs)

It also seems to be working with [Eta](https://eta-lang.org/) though the full test suite could not be run due to Eta's issues compiling `quickcheck` and `doctest`.

### Installation

Get the latest stable version from [hackage](https://hackage.haskell.org/package/flat).

### Acknowledgements

 `flat` reuses ideas and readapts code from various packages, mainly: `store`, `binary-bits` and `binary`.

### Known Bugs and Infelicities

* A performance issue with GHC 8.0.2 for some data types

* Longish compilation times for generated Flat instances


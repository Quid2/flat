[![Build Status](https://travis-ci.org/tittoassini/flat.svg?branch=master)](https://travis-ci.org/tittoassini/flat) [![Hackage version](https://img.shields.io/hackage/v/flat.svg)](http://hackage.haskell.org/package/flat)

Haskell implementation of [Flat](http://quid2.org/docs/Flat.pdf), a principled, portable and efficient binary data format ([specs](http://quid2.org)).

 ### How To Use It For Fun and Profit

To (de)serialise a data type, make it an instance of the `Flat` class.

There is `Generics` based support to automatically derive instances of additional types.

Let's see some code, we need a couple of extensions:

> {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
>> {-# LANGUAGE IncoherentInstances, NoMonomorphismRestriction #-}

Import the Flat library:

> import Data.Flat
>> import Data.Flat.Bits(bits,paddedBits)
>> import Data.Flat.Pretty(prettyShow)
>> import READMEUtil

Define a couple of custom data types, deriving Generic and Flat:

> data Direction = North | South | Center | East | West deriving (Show,Generic,Flat)

> data List a = Nil | Cons a (List a) deriving (Show,Generic,Flat)

For encoding, use `flat`, for decoding, use `unflat` (or equivalently: `flatStrict` and `unflatStrict`):

> d1 = unflatStrict . flat $ Cons North (Cons South Nil) :: Decoded (List Direction)

For the decoding to work correctly, you will naturally need to know the type of the serialised data. This is ok for applications that do not require long-term storage and that do not need to communicate across independently evolving agents. For those who do, you will need to supplement `flat` with something like [typed](https://github.com/tittoassini/typed).

 #### Define Instances for Abstract/Primitive types

 A set of primitives are available to define `Flat` instances for abstract or primitive types.

 Instances for some common, primitive or abstract data types (Bool,Words,Int,String,Text,ByteStrings,Tuples, Lists, Sequences, Maps ..) are already defined in [Data.Flat.Instances](https://github.com/tittoassini/flat/blob/master/src/Data/Flat/Instances.hs).

 #### Optimal Bit-Encoding

A pecularity of Flat is that it uses an optimal bit-encoding rather than the usual byte-oriented one.

 To see this, let's define a pretty printing function: `bits` encodes a value as a sequence of bits, `prettyShow` displays it nicely:

> p :: Flat a => a -> String
> p = prettyShow . bits

Now some encodings:

> p1 = p West

> p2 = p (Nil::List Direction)

> aList = Cons North (Cons South (Cons Center (Cons East (Cons West Nil))))
> p3 = p aList

As you can see, `aList` fits in less than 3 bytes rather than 11 as would be the case with other Haskell byte oriented serialisation packages like `binary` or `store`.

For the serialisation to work with byte-oriented devices or storage, we need to add some padding:

> f :: Flat a => a -> String
> f = prettyShow . paddedBits

> f1 = f West

> f2 = f (Nil::List Direction)

> f3 = f $ Cons North (Cons South (Cons Center (Cons East (Cons West Nil))))

The padding is a sequence of 0s terminated by a 1 running till the next byte boundary (if we are already at a byte boundary it will add an additional byte of value 1, that's unfortunate but there is a good reason for this, check the [specs](http://quid2.org/docs/Flat.pdf)).

Byte-padding is automatically added by the function `flat` and removed by `unflat`.

 ### Performance

For some hard data, see this [comparison of the major haskell serialisation libraries](https://github.com/haskell-perf/serialization).

Briefly:
 * Size: `flat` produces significantly smaller binaries than all other libraries (3/4 times usually)
 * Encoding: `store` and `flat` are usually faster
 * Decoding: `store`, `cereal` and `flat` are usually faster

 One thing that is not shown by the benchmarks is that, if the serialized data is to be transferred over a network, the total total transfer time (encoding time + transmission time + decoding time) is usually dominated by the transmission time and that's where the smaller binaries produced by flat give it a significant advantage.

 Consider for example the Cars dataset. As you can see in the following comparison with `store`, the overall top performer for encoding/decoding speed, the total transfer time is actually significantly lower for `flat` for all except the highest transmission speeds.

>> ccc1 = compareStoreFlat

 ### Haskell Compatibility

Tested with:
  * [ghc](https://www.haskell.org/ghc/) 7.10.3, 8.0.1 and 8.0.2 (x64)
  * [ghc](https://www.haskell.org/ghc/) 7.10.3/LLVM 3.5.2 (Arm7)
  * [ghcjs](https://github.com/ghcjs/ghcjs)

 ### Installation

Get the latest stable version from [hackage](https://hackage.haskell.org/package/flat).

 ### Acknowledgements

 `flat` reuses ideas and readapts code from various packages, mainly: `store`, `binary-bits` and `binary`.

 ### Known Bugs and Infelicities

* A performance issue with GHC 8.0.2 for some data types

* Longish compilation times for generated Flat instances



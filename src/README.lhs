[![Build Status](https://travis-ci.org/tittoassini/flat.svg?branch=master)](https://travis-ci.org/tittoassini/flat) [![Hackage version](https://img.shields.io/hackage/v/flat.svg)](http://hackage.haskell.org/package/flat)

Haskell implementation of [Flat](http://quid2.org), a principled and efficient binary data format ([specs](http://quid2.org/docs/Flat.pdf)).

 ### How To Use It For Fun and Profit

To (de)serialise a data type, make it an instance of the `Flat` class.

Instances for a few common data types (Bool,Tuples, Lists, String, Text ..) are already defined (in `Data.Flat.Instances`):

There is `Generics` based support to automatically derive instances of additional types.

Let's see some code.

Setup a couple of extensions:

> {-# LANGUAGE DeriveGeneric, DeriveAnyClass, NoMonomorphismRestriction #-}

Import the Flat library:

> import Data.Flat
>> import Data.Flat.Bits(bits,paddedBits)
>> import Data.Flat.Pretty(prettyShow)
>> import READMEUtil

Define a couple of custom data types, deriving `Generic` and `Flat`:

> data Direction = North | South | Center | East | West deriving (Show,Generic,Flat)
> data List a = Nil | Cons a (List a) deriving (Show,Generic,Flat)

For encoding, use 'flat', for decoding, use `unflat`:

> d1 = unflat . flat $ Cons North (Cons South Nil) :: Decoded (List Direction)

For the decoding to work correctly, you will naturally need to know the type of the serialised data. This is ok for certain applications. For the rest, you will need to supplement `flat` with something like [typed](https://github.com/tittoassini/typed).


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

For the serialisation to work with byte-oriented devices or storage, we need to add some padding (this is done automatically by the function `flat`):

> f :: Flat a => a -> String
> f = prettyShow . paddedBits

> f1 = f West

> f2 = f (Nil::List Direction)

> f3 = f $ Cons North (Cons South (Cons Center (Cons East (Cons West Nil))))

The padding is a sequence of 0s terminated by a 1 running till the next byte boundary (if we are already at a byte boundary it will add an additional byte of value 1, that's unfortunate but there is a good reason for this, check the [specs](http://quid2.org/docs/Flat.pdf)).


 ### Performance

For some hard data, see this [comparison of the major haskell serialisation libraries](https://github.com/haskell-perf/serialization).

Briefly:
 * Size: 'flat' produces significantly smaller binaries than other libraries (3/4 times usually)
 * Encoding: 'store' is usually faster, followed by 'flat' and 'cereal'
 * Decoding: 'store' is usually faster, followed by 'flat' and 'cereal'

 One thing that is not shown by the benchmarks is that, if the serialized data is to be transferred over a network, the total total transfer times (encoding time + transmission time + decoding time) is dominated by the transmission time and that's where the smaller binaries produced by flat give it a significant advantage.

 Consider for example the Cars dataset, as you can see in the following comparison with `store`, the top performer among the binary serialisation packages, that the total transfer time is actually significantly lower for `flat` for all except the highest transmission speeds.

>> q1 = compareStoreFlat


 ### Haskell Compatibility

Tested with:
  * [ghc](https://www.haskell.org/ghc/) 7.10.3, 8.0.1 and 8.0.2 (x64)
  * [ghc](https://www.haskell.org/ghc/) 7.10.3/LLVM 3.5.2 (Arm7)
  * [ghcjs](https://github.com/ghcjs/ghcjs)

 ### Installation

It is not yet on [hackage](https://hackage.haskell.org/) but you can use it in your [stack](https://docs.haskellstack.org/en/stable/README/) projects by adding in the `stack.yaml` file, under the `packages` section:

````
- location:
   git: https://github.com/tittoassini/flat
   commit: a0fbd3763756ea5fad7f6d8c8e0354488c22811e
  extra-dep: true
````

 ### Known Bugs and Infelicities

* Long compilation times for generated Flat instances

-----
[Source code](https://github.com/tittoassini/flat/blob/master/src/README.lhs)

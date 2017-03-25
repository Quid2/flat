Haskell implementation of [Flat](http://quid2.org), a minimalist binary data format ([specs](http://quid2.org/docs/Flat.pdf)).

 ### How To Use It For Fun and Profit

Flat is a binary data format, similar to `binary` or `cereal`.

To (de)serialise a data type, make it an instance of the `Flat` class.

Instances for a few common data types (Bool,Tuples, Lists, String, Text ..) are already defined (in `Data.Flat.Instances`):

There is `Generics` based support to automatically derive instances of additional types.

Let's see some code.

Setup a couple of extensions:

> {-# LANGUAGE DeriveGeneric, DeriveAnyClass, NoMonomorphismRestriction #-}

Import the Flat library:

> import Data.Flat
> import Data.Flat.Bits
> import Data.Flat.Pretty

Define a couple of custom data types, deriving `Generic` and `Flat`:

> data Direction = North | South | Center | East | West deriving (Show,Generic,Flat)
> data List a = Nil | Cons a (List a) deriving (Show,Generic,Flat)

Define a utility function: `bits` encodes the value as a sequence of bits, `prettyShow` displays it nicely:

> p = prettyShow . valueBits

Some encodings:

> p1 = p West

> p2 = p (Nil::List Direction)

> aList = Cons North (Cons South (Cons Center (Cons East (Cons West Nil))))
> p3 = p aList

These encodings shows a pecularity of Flat, it uses an optimal bit-encoding rather than the usual byte-oriented one (so that `aList` fits in less than 3 bytes rather than 11).

For the serialisation to work with byte-oriented devices or storage, we need to add some padding, this is done automatically by the function `flat`:

> f :: Flat a => a -> String
> f = prettyShow . bits . flat

> f1 = f West

> f2 = f (Nil::List Direction)

> f3 = f $ Cons North (Cons South (Cons Center (Cons East (Cons West Nil))))

The padding is a sequence of 0s terminated by a 1 running till the next byte boundary (if we are already at a byte boundary it will add an additional byte of value 1, that's unfortunate but there is a good reason for this, check the [specs](http://quid2.org/docs/Flat.pdf)).

For decoding, use `unflat`:

> d1 = unflat . flat $ Cons North (Cons South Nil) :: Decoded (List Direction)

 ### Installation

It is not yet on [hackage](https://hackage.haskell.org/) but you can use it in your [stack](https://docs.haskellstack.org/en/stable/README/) projects by adding in the `stack.yaml` file, under the `packages` section:

````
- location:
   git: https://github.com/tittoassini/flat
   commit: 9734f1b5702d531483b374b541d5fb8f235875cf
  extra-dep: true
````

 ### Haskell Compatibility

Tested with [ghc](https://www.haskell.org/ghc/) 7.10.3, 8.0.1 and 8.0.2 (x64).

 ### Performance

See this [comparison of the major haskell serialisation libraries](https://github.com/haskell-perf/serialization).

Briefly:
 * Significantly most compact than other libraries (3/4 times usually)
 * Encoding is usually faster than 'binary' ..
 * Decoding ..

 Brief summary: flat produces significantly more compact binary representation.

 ? flat is usually faster than ?

 Tips in :

 a)
 Define instances of parametric data types as OVERLAPPABLE
 instance {-# OVERLAPPABLE #-} Flat a => Flat (Tree a)

-- Or do not define them at all and just add instances of concrete types when necessary

Define also

data SomeLists = SomeLists [Bool] [Int]

The compiler won't create specialised [Bool] and [Int], by adding them we get some extra speed.

 -- Almost 2x faster
 instance {-# OVERLAPPING #-} Flat (Tree N)

But this creates toruble in cases like:

data Content user message = Users [User]

Solution: define Flat instances only for concrete types

 b) Writing instances by hand (really? why?):

  Difficult to do:
  encode = \case
    One -> eBits 2 0
    Two ->  eBits 2 1
    Three -> eBits 2 2
    Four -> eBits 3 6
    Five -> eBits 3 7

 define: eHeader num totNum

 instance {-# OVERLAPPING #-} Flat (Tree N) where
    encode (Node t1 t2) = eFalse <> encode t1 <> encode t2
    encode (Leaf a) = eTrue <> encode a


 ### Known Bugs and Infelicities

* Encoding and decoding of String/Text and Float/Double is very slow
* Contains code 'lifted' and modified from the binary-bits package
* Messy source code
* Long compilation times for generated Flat instances

-----
[Source code](https://github.com/tittoassini/flat/blob/master/src/README.lhs)

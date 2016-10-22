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
> import Data.Flat.Pretty

Define a couple of custom data types, deriving `Generic` and `Flat`:

> data Direction = North | South | Center | East | West deriving (Show,Generic,Flat)
> data List a = Nil | Cons a (List a) deriving (Show,Generic,Flat)

Define a utility function: `bits` encodes the value, `prettyShow` displays it nicely:

> p = prettyShow . bits

Some encodings:

> p1 = p West

> p2 = p (Nil::List Direction)

> aList = Cons North (Cons South (Cons Center (Cons East (Cons West Nil))))
> p3 = p aList

These encodings shows a pecularity of Flat, it uses an optimal bit-encoding rather than the usual byte-oriented one (so that `aList` fits in less than 3 bytes rather than 11).

For the serialisation to work with byte-oriented devices, we need to add some padding, this is done automatically by the function `flat`:

> f :: Flat a => a -> String
> f = prettyShow . flat

> f1 = f West

> f2 = f (Nil::List Direction)

> f3 = f $ Cons North (Cons South (Cons Center (Cons East (Cons West Nil))))

The padding is a sequence of 0s terminated by a 1 (why? check the [specs](http://quid2.org/docs/Flat.pdf)) running till the next byte boundary.

For decoding, use `unflat`:

> d1 = unflat . flat $ Cons North (Cons South Nil) :: Decoded (List Direction)

 ### Installation

It is not yet on [hackage](https://hackage.haskell.org/) so to use in your [stack](https://docs.haskellstack.org/en/stable/README/) projects, add a reference to its github location under the 'packages' section:

````
packages:
- location:
    git: https://github.com/tittoassini/flat
    commit:
````

 ### Compatibility

Tested with [ghc](https://www.haskell.org/ghc/) 7.10.3 and 8.0.1.

 ### Known Bugs and Infelicities

* Encoding and decoding of String/Text is very slow
* Messy source code
* Contains code 'lifted' and modified from the binary-bits package
-----
[Source code](https://github.com/tittoassini/flat/blob/master/src/README.lhs)

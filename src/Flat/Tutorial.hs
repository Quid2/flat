module Flat.Tutorial
  (
    -- $setup

    -- $main
  )
where


{- $setup
To (de)serialise a data type, make it an instance of the 'Flat.Class.Flat' class.

There is <https://hackage.haskell.org/package/base/docs/GHC-Generics.html Generics> based support to automatically derive a correct instance.

Let’s see some code.

We need a couple of extensions:

>>> :set -XDeriveGeneric -XDeriveAnyClass

The @Flat@ top module:

>>> import Flat

And, just for fun, a couple of functions to display an encoded value as a sequence of bits:

>>> import Flat.Instances.Test (flatBits,allBits)

Define a few custom data types, deriving @Generic@ and @Flat@:

>>> data Result = Bad | Good deriving (Show,Generic,Flat)

>>> data Direction = North | South | Center | East | West deriving (Show,Generic,Flat)

>>> data List a = Nil | Cons a (List a) deriving (Show,Generic,Flat)
-}

{- $main
Now we can encode a List of Directions using 'Flat.Run.flat':

>>> flat $ Cons North (Cons South Nil)
"\149"

The result is a strict <https://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html ByteString>.

And decode it back using 'Flat.Run.unflat':

>>> unflat . flat $ Cons North (Cons South Nil) :: Decoded (List Direction)
Right (Cons North (Cons South Nil))

The result is a 'Flat.Decoded' value: 'Either' a 'Flat.DecodeException' or the actual value.

=== Optimal Bit-Encoding
#optimal-bit-encoding#

A pecularity of Flat is that it uses an optimal bit-encoding rather than
the usual byte-oriented one.

One bit is all we need for a 'Result' or for an empty 'List' value:

>>> flatBits Good
"1"

>>> flatBits (Nil::List Direction)
"0"

Two or three bits suffice for a 'Direction' value:

>>> flatBits South
"01"

>>> flatBits West
"111"

For the serialisation to work with byte-oriented devices or storage, we need to add some padding.

To do so, rather than encoding a plain value, 'Flat.Run.flat' encodes a 'Flat.Filler.PostAligned' value, that's to say a value followed by a 'Flat.Filler.Filler' that stretches till the next byte boundary.

In practice, the padding is a, possibly empty, sequence of 0s followed by a 1.

For example, this list encodes as 7 bits:

>>> flatBits $ Cons North (Cons South Nil)
"1001010"

And, with the added padding of a final "1", will snugly fit in a single byte:

>>> allBits $ Cons North (Cons South Nil)
"10010101"

But .. you don't need to worry about these details as byte-padding is automatically added by the function 'Flat.Run.flat' and removed by 'Flat.Run.unflat'.

=== Pre-defined Instances

Flat instances are already defined for relevant types of some common packages: array, base, bytestring, containers, dlist, mono-traversable, text, unordered-containers, vector.

They are automatically imported by the "Flat" module.

For example:

>>> flatBits $ Just True
"11"
-}



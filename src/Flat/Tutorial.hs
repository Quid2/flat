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

The @Flat@ library:

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

But .. you don't need to worry about these details as byte-padding is automatically added by the function 'Flat.Run.flat' and removed by 'Flat.Run.unflat'.

For example, this encodes as 7 bits:

>>> flatBits $ Cons North (Cons South Nil)
"1001010"

And, with the added padding of a final "1", will snugly fit in a single byte:

>>> allBits $ Cons North (Cons South Nil)
"10010101"

=== Pre-defined Instances

Flat instances are already defined for relevant types of some common packages: array, base, bytestring, containers, dlist, mono-traversable, text, unordered-containers, vector.

They are automatically imported by the "Flat" module.

== Compatibility
#compatibility#

=== <https://www.haskell.org/ghc/ GHC>
#ghc#

Tested with: <https://www.haskell.org/ghc/ ghc> 7.10.3, 8.0.2, 8.2.2, 8.4.4, 8.6.5 and 8.8.3 (x64).

Should also work with (not recently tested): <https://www.haskell.org/ghc/ ghc> 7.10.3\/LLVM 3.5.2 (Arm7).

=== <https://github.com/ghcjs/ghcjs GHCJS>
#ghcjs#

Passes all tests in the @flat@ testsuite, except for those relative to short <https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Short.html ByteString> that is unsupported by @ghcjs@.

Check <https://github.com/Quid2/flat/blob/master/stack-ghcjs.yaml stack-ghcjs.yaml> to see with what versions of @ghcjs@ it has been tested.

If you use a different version of @ghcjs@, you might want to run the test suite by setting your compiler in <https://github.com/Quid2/flat/blob/master/stack-ghcjs.yaml stack-ghcjs.yaml>
and then running:

@stack test --stack-yaml=stack-ghcjs.yaml@

NOTE: Versions of @flat@ prior to 0.33 encoded @Double@ values incorrectly when they are not aligned with a byte boundary.

=== <https://eta-lang.org/ ETA>
#eta#

It builds (with etlas 1.5.0.0 and eta eta-0.8.6b2) but currently fails the test suite.

Support for eta is not currently being actively mantained.

== Known Bugs and Infelicities
#known-bugs-and-infelicities#

=== Longish compilation times

Relies more than other serialisation libraries on extensive inlining for its good performance, this unfortunately leads to longer compilation times.

If you have many data types or very large ones this might become an issue.

A couple of good practices that will eliminate or mitigate this problem are:

-   During development, turn optimisations off (@stack --fast@ or @-O0@
    in the cabal file).

-   Keep your serialisation code in a separate module(s).

=== Data types with more than 512 constructors are currently unsupported

This limit could be easily extended, shout if you need it.

=== Other

<https://github.com/Quid2/flat/issues Full list of open issues>.

== Other Stuff You Might Like

=== <https://github.com/Quid2/zm ZM - Language independent, reproducible, absolute types>

To decode @flat@ encoded data you need to know the type of the serialised data.

This is ok for applications that do not require long-term storage and that do not operate in open distributed systems.

For those who do, you might want to supplement @flat@ with something like <https://github.com/Quid2/zm zm>.

=== Ports for other languages

<https://github.com/Quid2/ts TypeScript\/JavaScript> and <https://www.purescript.org/ Purescript> ports are under development.

Get in touch if you would like to help porting @Flat@ to other languages.

== Acknowledgements
#acknowledgements#

@flat@ reuses ideas and readapts code from various packages, mainly:
@store@, @binary-bits@ and @binary@ and includes contributions from
Justus Sagemüller.
-}



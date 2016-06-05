Haskell implementation of [Flat](http://quid2.org), a minimalist binary data format ([specs](http://quid2.org/docs/Flat.pdf)).

 ### Installation

Install as part of the [quid2](https://github.com/tittoassini/quid2) project.

 ### Brief Tutorial for Haskellers

Flat is a binary data format, similar to `binary` or `cereal`.

To (de)serialise a data type it needs to be an instance of the 'Flat' class.

There is `Generics` based support to automatically define instances.

So, let's enable `Generics`:

> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

Import the Flat library:

> import Data.Flat

Define a couple of custom data types:

> data Direction = North | South | Center | East | West deriving (Show,Generic)

> data List a = Cons a (List a) | Nil deriving (Show,Generic)

Automatically derive the instances:

> instance Flat Direction
> instance Flat a => Flat (List a)

A little utility function, `bits` encodes the value, `prettyShow` displays it nicely:

> pp :: Flat a => a -> String
> pp = prettyShow . bits

> e1 = pp Center

> e2 = pp (Nil::List Direction)

> e3 = pp $ Cons North (Cons South Nil)

These encodings shows a pecularity of Flat, it uses an optimal bit-encoding rather than more usual byte-oriented one.

Instances for a few common data types (Bool,Tuples, Lists, String, Text ..) are already defined (in `Data.Flat.Instances):

-- Serialize a value
e1 = encoded $ Couple One Due
-- One has been encoded as '00', Due as '01', the rest is byte-padding.

-- Now get it back
d1 = decoded e1 :: Decoded (Couple Number Numero)

-- One more time
d2 :: Decoded (Couple Numero Number)
d2 = decoded . Encoded . bytes $ e1

See the [source code](http://github.com/tittoassini/flat/src/README.lhs) of this file. 

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

Some encodings:

> e1 = pp Center

> e2 = pp (Nil::List Direction)

> e3 = pp $ Cons North (Cons South Nil)

These encodings shows a pecularity of Flat, it uses an optimal bit-encoding rather than more usual byte-oriented one.

... to be continued 

See the [source code](https://github.com/tittoassini/flat/src/README.lhs) of this file. 

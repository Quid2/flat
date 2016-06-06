
Haskell implementation of [Flat](http://quid2.org), a minimalist binary data format ([specs](http://quid2.org/docs/Flat.pdf)).

### Installation

Install as part of the [quid2](https://github.com/tittoassini/quid2) project.

### Brief Tutorial for Haskellers

Flat is a binary data format, similar to `binary` or `cereal`.

To (de)serialise a data type it needs to be an instance of the `Flat` class.

Instances for a few common data types (Bool,Tuples, Lists, String, Text ..) are already defined (in `Data.Flat.Instances`):

There is `Generics` based support to automatically derive instances of additional types.

So, let's enable `Generics`:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
```

Import the Flat library:

```haskell
import Data.Flat
```

Define a couple of custom data types, deriving `Generic`:

```haskell
data Direction = North | South | Center | East | West deriving (Show,Generic)
```

```haskell
data List a = Cons a (List a) | Nil deriving (Show,Generic)
```

Automatically derive the `Flat` instances:

```haskell
instance Flat Direction
instance Flat a => Flat (List a)
```

A little utility function: `bits` encodes the value, `prettyShow` displays it nicely:

```haskell
p :: Flat a => a -> String
p = prettyShow . bits
```

Let's see some encodings:

```haskell
p1 = p West
```
p1 -> "111"

```haskell
p2 = p (Nil::List Direction)
```
p2 -> "1"

```haskell
p3 = p $ Cons North (Cons South (Cons Center (Cons East (Cons West Nil))))
```
p3 -> "00000101 00110011 11"

These encodings shows a pecularity of Flat, it uses an optimal bit-encoding rather than the usual byte-oriented one.

For the serialisation to work with byte-oriented devices, we need to add some padding, this is done automatically by the function `flat`:

```haskell
f :: Flat a => a -> String
f = prettyShow . flat
```

```haskell
f1 = f West
```
f1 -> "11100001"
>

```haskell
f2 = f (Nil::List Direction)
```
f2 -> "10000001"

```haskell
f3 = f $ Cons North (Cons South (Cons Center (Cons East (Cons West Nil))))
```
f3 -> "00000101 00110011 11000001"

The padding is a sequence of 0s terminated by a 1, till the next byte boundary.

For decoding, use `unflat`:

```haskell
d1 = unflat (flat $ Cons North (Cons South Nil)) :: Decoded (List Direction)
```
d1 -> Right (Cons North (Cons South Nil))

-----
[Source code](https://github.com/tittoassini/flat/blob/master/src/README.lhs). 

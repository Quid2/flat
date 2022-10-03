{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Flat.Repr where

import qualified Data.ByteString    as B
import           Flat.Class         (Flat (..))
import           Flat.Decoder.Types (Get)
import           Flat.Run           (flat, unflat)

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> import Flat.Instances.Base
-- >>> import Flat.Decoder.Types
-- >>> import Flat.Types
-- >>> import Flat.Run
-- >>> import Flat.Class

{- | Flat representation of a value

It is occasionally useful to keep a decoded value, or part of it, in its encoded binary representation and decode it later on demand.

To do so, just decode a value `a` to a `Repr a`.

For example, we encode a list of Ints and then decode it to a list of Repr Int:

>>> unflat (flat [1::Int .. 5]) :: Decoded ([Repr Int])
Right [Repr {repr = "\STX\SOH"},Repr {repr = "\EOT\SOH"},Repr {repr = "\ACK\SOH"},Repr {repr = "\b\SOH"},Repr {repr = "\n\SOH"}]

To decode a `Repr a` to an `a`, we use `unrepr`:

>>> let Right l = unflat (flat [1..5]) :: Decoded [Repr Int] in unrepr (l  !! 2)
3

See test/FlatRepr.hs for a test and a longer example of use.


If a decoded value is not required, it can be skipped completely using `SizeOf a`.

For example, if we are not interested in the second and fourth component of this encoded tuple:

>>> let v = flat ('a',"abc",'z',True)

We can decode it as:

>>> unflat v :: Decoded (Char,SizeOf String,Char,SizeOf Bool)
Right ('a',SizeOf 28,'z',SizeOf 1)

The unused values have not been decoded and instead their size (in bits) has been returned.
-}

newtype Repr a = Repr {repr :: B.ByteString} deriving Show

-- Get the underlying value
unrepr :: Flat a => Repr a -> a
unrepr (Repr bs)=
    case unflat bs of
        Right a -> a
        Left _  -> error "impossible"

instance Flat a => Flat (Repr a) where
    size = error "unused"
    encode = error "unused"

    -- To create the representation we just re 'flat' the parsed value (this could be optimised by copying directly the parsed representation)
    decode = Repr . flat <$> (decode :: Get a)

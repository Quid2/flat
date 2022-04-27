{-# LANGUAGE ScopedTypeVariables #-}
module Flat.Repr where

import Flat.Class ( Flat(..) )
import qualified Data.ByteString as B
import Flat.Decoder.Types ( Get )
import Flat.Run ( flat, unflat )

-- Flat representation of a value
newtype Repr a = Repr {repr :: B.ByteString}

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


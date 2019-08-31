{-# LANGUAGE CPP          #-}
-- |Strict Decoder
module Data.Flat.Decoder (
    strictDecoder,
    strictDecoderPart,
    Decoded,
    DecodeException,
    Get,
    dByteString,
    dLazyByteString,
    dShortByteString,
    dShortByteString_,
#ifndef ghcjs_HOST_OS
    dUTF16,
#endif
    dUTF8,
    decodeArrayWith,
    decodeListWith,
    dFloat,
    dDouble,
    dInteger,
    dNatural,
    dChar,
    dBool,
    dWord8,
    dWord16,
    dWord32,
    dWord64,
    dWord,
    dInt8,
    dInt16,
    dInt32,
    dInt64,
    dInt,
    dBE8,
    dBE16,
    dBE32,
    dBE64,
    dBEBits8,
    dBEBits16,
    dBEBits32,
    dBEBits64,
    dropBits,

    ConsState(..),consOpen,consClose,consBool,consBits
    ) where

import           Data.Flat.Decoder.Prim
import           Data.Flat.Decoder.Strict
import           Data.Flat.Decoder.Types

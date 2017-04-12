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
    dUTF16,
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
    ) where

import Data.Flat.Decoder.Prim
import Data.Flat.Decoder.Strict
import Data.Flat.Decoder.Types

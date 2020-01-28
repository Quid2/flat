{-# LANGUAGE CPP   ,NoMonomorphismRestriction    #-}
module Data.Flat.Encoder (
    Encoding,
    (<>),
    NumBits,
    encodersS,
    mempty,
    strictEncoder,
    eTrueF,
    eFalseF,
    eFloat,
    eDouble,
    eInteger,
    eNatural,
    eWord16,
    eWord32,
    eWord64,
    eWord8,
    eBits,
    eBits16,
    eFiller,
    eBool,
    eTrue,
    eFalse,
    eBytes,
#ifndef ghcjs_HOST_OS
    eUTF16,
#endif
    eLazyBytes,
    eShortBytes,
    eInt,
    eInt8,
    eInt16,
    eInt32,
    eInt64,
    eWord,
    eChar,
    encodeArrayWith,
    encodeListWith,
    Size,
    arrayBits,
    sWord,
    sWord8,
    sWord16,
    sWord32,
    sWord64,
    sInt,
    sInt8,
    sInt16,
    sInt32,
    sInt64,
    sNatural,
    sInteger,
    sFloat,
    sDouble,
    sChar,
    sBytes,
    sLazyBytes,
    sShortBytes,
#ifndef ghcjs_HOST_OS
    sUTF16,
#endif
    sFillerMax,
    sBool,
    sUTF8Max,
    eUTF8,
#ifdef ETA_VERSION
    trampolineEncoding,
#endif
    ) where

import           Data.Flat.Encoder.Prim
import           Data.Flat.Encoder.Size(arrayBits)
import           Data.Flat.Encoder.Strict
import           Data.Flat.Encoder.Types

-- import           Data.Semigroup((<>))

#if ! MIN_VERSION_base(4,9,0)
import           Data.Semigroup((<>))
-- (<>) = mappend
#endif

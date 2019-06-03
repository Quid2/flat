{-# LANGUAGE CPP #-}
-- | Endian utilities
-- Exported for testing purposes, but not meant to be used outside this package.
module Data.Flat.Endian
    (
    toBE32
    , toBE64
    , toBE16
    , isBigEndian
    , fix64
    ) where

#include "MachDeps.h"

import Data.Word

#ifdef ghcjs_HOST_OS
import Data.Bits
#endif

isBigEndian :: Bool
isBigEndian =
#if defined(WORDS_BIGENDIAN) || defined(ETA_VERSION)
    True
#else
    False
#endif

-- | Convert a 64 bit value in cpu endianess to big endian
toBE64 :: Word64 -> Word64
#if defined(WORDS_BIGENDIAN) || defined(ETA_VERSION)
toBE64 = id
#else
toBE64 = byteSwap64
#endif

-- | Convert a 32 bit value in cpu endianess to big endian
toBE32 :: Word32 -> Word32
#if defined(WORDS_BIGENDIAN) || defined(ETA_VERSION)
toBE32 = id
#else
toBE32 = byteSwap32
#endif

-- | Convert a 16 bit value in cpu endianess to big endian
toBE16 :: Word16 -> Word16
#if defined(WORDS_BIGENDIAN) || defined(ETA_VERSION)
toBE16 = id
#else
toBE16 = byteSwap16
#endif

-- | Fix issue with `ghcjs` (different order of 32 bit halves of 64 values with respect to `ghc`)
fix64 :: Word64 -> Word64
#ifdef ghcjs_HOST_OS
fix64 = (`rotateR` 32)
{-# NOINLINE fix64 #-}
#else
fix64 = id
{-# INLINE fix64 #-}
#endif

{-# LANGUAGE CPP #-}
module System.Endian
    (
    toBE32
    , toBE64
    , toBE16
    , isBigEndian
    ) where

#include "MachDeps.h"

import Data.Word

isBigEndian :: Bool
isBigEndian = 
    #ifdef WORDS_BIGENDIAN
    True
    #else
    False
    #endif

-- | Convert a 64 bit value in cpu endianess to big endian
toBE64 :: Word64 -> Word64
#ifdef WORDS_BIGENDIAN
toBE64 = id
#else
toBE64 = byteSwap64
#endif

-- | Convert a 32 bit value in cpu endianess to big endian
toBE32 :: Word32 -> Word32
#ifdef WORDS_BIGENDIAN
toBE32 = id
#else
toBE32 = byteSwap32
#endif

-- | Convert a 16 bit value in cpu endianess to big endian
toBE16 :: Word16 -> Word16
#ifdef WORDS_BIGENDIAN
toBE16 = id
#else
toBE16 = byteSwap16
#endif

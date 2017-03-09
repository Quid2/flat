module Data.Flat.Peeks(Get,dBool,dWord8,dWord32be,dWord64be,dByteString_) where

import           Data.Binary.Bits.Get
import           Data.ByteString
import           Data.Word

{-# INLINE dBool #-}
dBool :: Get Bool
dBool = getBool

{-# INLINE dWord8  #-}
dWord8 :: Get Word8
dWord8 = getWord8 8

{-# INLINE dWord32be  #-}
dWord32be :: Get Word32
dWord32be = getWord32be 32

{-# INLINE dWord64be  #-}
dWord64be :: Get Word64
dWord64be = getWord64be 64

{-# INLINE dByteString_  #-}
dByteString_ :: Int -> Get ByteString
dByteString_ = getByteString


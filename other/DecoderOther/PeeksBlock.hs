module Data.Flat.Peeks (
    Get,
    dBool,
    dWord8,
    dFloat,dDouble,
    -- dWord32be,
    --dWord64be,
    dByteString_,
    runGetLazy
    ) where

import           Data.Binary.Bits.Get
import           Data.ByteString
import           Data.Word
import qualified Data.ByteString.Lazy as L

type Get = Block

runGetLazy :: Get b -> L.ByteString -> Either String (b, L.ByteString, Int)
runGetLazy get bs = runPartialGet (block get) bs 0

{-# INLINE dBool #-}
dBool :: Get Bool
dBool = bool

{-# INLINE dWord8  #-}
dWord8 :: Get Word8
dWord8 = byte

{-# INLINE dFloat #-}
dFloat :: Get Float
dFloat = float

{-# INLINE dDouble #-}
dDouble :: Get Double
dDouble = double

-- {-# INLINE dWord32be  #-}
-- dWord32be :: Get Word32
-- dWord32be = getWord32be 32

-- {-# INLINE dWord64be  #-}
-- dWord64be :: Get Word64
-- dWord64be = getWord64be 64

{-# INLINE dByteString_  #-}
dByteString_ :: Int -> Get ByteString
dByteString_ = byteString


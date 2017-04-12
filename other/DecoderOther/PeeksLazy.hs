module Data.Flat.Peeks (
    Get,
    dBool,
    dWord8,
    dFloat,dDouble,
    dByteString_,
    runGetLazy
    ) where

import           Data.Binary.Bits.Get
-- import           Data.ByteString
import           Data.Word
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

type Get = BitGet

runGetLazy :: Get b -> L.ByteString -> Either String (b, L.ByteString, Int)
runGetLazy get bs = runPartialGet get bs 0

{-# INLINE dBool #-}
dBool :: Get Bool
dBool = getBool

{-# INLINE dWord8  #-}
dWord8 :: Get Word8
dWord8 = getByte

{-# INLINE dFloat #-}
dFloat :: Get Float
dFloat = getFloat

{-# INLINE dDouble #-}
dDouble :: Get Double
dDouble = getDouble

-- {-# INLINE dWord32be  #-}
-- dWord32be :: Get Word32
-- dWord32be = getWord32be 32

-- {-# INLINE dWord64be  #-}
-- dWord64be :: Get Word64
-- dWord64be = getWord64be 64

{-# INLINE dByteString_  #-}
--dByteString_ :: Int -> Get ByteString
-- dByteString_ = getByteString

dByteString_ :: Get B.ByteString
dByteString_ = B.concat <$> dBytes_

dBytes_ :: Get [B.ByteString]
dBytes_ =  do
  l <- dWord8
  if l==0
    then return []
    else do
       bs <- getByteString (fromIntegral l)
       bs' <- dBytes_
       return $ bs : bs'

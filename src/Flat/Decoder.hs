{-# LANGUAGE CPP          #-}
-- |Strict Decoder
module Flat.Decoder (
    strictDecoder,
    decodeListTWith,
    -- strictDecoderPart,
    Decoded,
    DecodeException(..),
    Get,
    dByteString,
    dLazyByteString,
    dShortByteString,
    dShortByteString_,
#if! defined(ghcjs_HOST_OS) && ! defined (ETA_VERSION)
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

import           Flat.Decoder.Prim
import           Flat.Decoder.Strict
import           Flat.Decoder.Types
import           Foreign

import ListT
import qualified Data.ByteString.Internal as BS

decodeListTWith :: Get a -> BS.ByteString -> IO (ListT IO a)
decodeListTWith get (BS.PS base off len) = 
    withForeignPtr base $ \base0 -> do
        let ptr = base0 `plusPtr` off
            endPtr = ptr `plusPtr` len
            s = S ptr 0
            go s = do
                GetResult s' b <- runGet dBool endPtr s
                if b
                    then do
                        GetResult s'' a <- runGet get endPtr s'
                        return $ Just (a, ListT $ go s'')
                    else return Nothing
        return $ ListT (go s)

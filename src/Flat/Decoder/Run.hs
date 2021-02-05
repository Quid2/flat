module Flat.Decoder.Run(strictDecoder,listTDecoder) where

import Foreign ( Ptr, plusPtr, withForeignPtr )
import qualified Data.ByteString          as B
import ListT ( ListT(..) )
import qualified Data.ByteString.Internal as BS
import Control.Exception ( try, Exception )
import Flat.Decoder.Types
    ( tooMuchSpace, S(S), GetResult(..), Get(runGet), DecodeException )
import System.IO.Unsafe ( unsafePerformIO )
import Flat.Decoder.Prim ( dBool )

-- | Given a decoder and an input buffer returns either the decoded value or an error  (if the input buffer is not fully consumed) 
strictDecoder :: Get a -> B.ByteString -> Either DecodeException a
strictDecoder get bs =
  strictDecoder_ get bs $ \(GetResult s'@(S ptr' o') a) endPtr ->
    if ptr' /= endPtr || o' /= 0
      then tooMuchSpace endPtr s'
      else return a

strictDecoder_ ::
     Exception e
  => Get a1
  -> BS.ByteString
  -> (GetResult a1 -> Ptr b -> IO a)
  -> Either e a
strictDecoder_ get (BS.PS base off len) check =
  unsafePerformIO . try $
  withForeignPtr base $ \base0 ->
    let ptr = base0 `plusPtr` off
        endPtr = ptr `plusPtr` len
     in do res <- runGet get endPtr (S ptr 0)
           check res endPtr
{-# NOINLINE strictDecoder_ #-}


-- strictRawDecoder :: Exception e => Get t -> B.ByteString -> Either e (t,B.ByteString, NumBits)
-- strictRawDecoder get (BS.PS base off len) = unsafePerformIO . try $
--   withForeignPtr base $ \base0 ->
--     let ptr = base0 `plusPtr` off
--         endPtr = ptr `plusPtr` len
--     in do
--       GetResult (S ptr' o') a <- runGet get endPtr (S ptr 0)
--       return (a, BS.PS base (ptr' `minusPtr` base0) (endPtr `minusPtr` ptr'), o')


listTDecoder :: Get a -> BS.ByteString -> IO (ListT IO a)
listTDecoder get (BS.PS base off len) = 
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


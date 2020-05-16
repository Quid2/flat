{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}

-- |Strict Decoder Types
module Flat.Decoder.Types
  ( strictDecoder
  , strictDecoderPart
  , Get(..)
  , S(..)
  , GetResult(..)
  , Decoded
  , DecodeException(..)
  , notEnoughSpace
  , tooMuchSpace
  , badEncoding
  ) where

import           Control.DeepSeq
import           Control.Exception
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BS
import           Data.Word
import           Foreign
import           System.IO.Unsafe
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail       as Fail
#endif
strictDecoder :: Get a -> B.ByteString -> Either DecodeException a
strictDecoder get bs =
  strictDecoder_ get bs $ \(GetResult s'@(S ptr' o') a) endPtr ->
    if ptr' /= endPtr || o' /= 0
      then tooMuchSpace endPtr s'
      else return a

strictDecoderPart :: Get a -> B.ByteString -> Either DecodeException a
strictDecoderPart get bs =
  strictDecoder_ get bs $ \(GetResult _ a) _ -> return a

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

-- strictRawDecoder :: Exception e => Get t -> B.ByteString -> Either e (t,B.ByteString, NumBits)
-- strictRawDecoder get (BS.PS base off len) = unsafePerformIO . try $
--   withForeignPtr base $ \base0 ->
--     let ptr = base0 `plusPtr` off
--         endPtr = ptr `plusPtr` len
--     in do
--       GetResult (S ptr' o') a <- runGet get endPtr (S ptr 0)
--       return (a, BS.PS base (ptr' `minusPtr` base0) (endPtr `minusPtr` ptr'), o')
-- |Decoder monad
newtype Get a =
  Get
    { runGet :: Ptr Word8 -- End Ptr
                 -> S -> IO (GetResult a)
    } -- deriving (Functor)

-- Seems to give better performance than the derived version
instance Functor Get where
  fmap f g =
    Get $ \end s -> do
      GetResult s' a <- runGet g end s
      return $ GetResult s' (f a)
  {-# INLINE fmap #-}

-- Is this correct?
instance NFData (Get a) where
  rnf !_ = ()

instance Show (Get a) where
  show _ = "Get"

instance Applicative Get where
  pure x = Get (\_ ptr -> return $ GetResult ptr x)
  {-# INLINE pure #-}
  Get f <*> Get g =
    Get $ \end ptr1 -> do
      GetResult ptr2 f' <- f end ptr1
      GetResult ptr3 g' <- g end ptr2
      return $ GetResult ptr3 (f' g')
  {-# INLINE (<*>) #-}
  Get f *> Get g =
    Get $ \end ptr1 -> do
      GetResult ptr2 _ <- f end ptr1
      g end ptr2
  {-# INLINE (*>) #-}

instance Monad Get where
  return = pure
  {-# INLINE return #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}
  Get x >>= f =
    Get $ \end s -> do
      GetResult s' x' <- x end s
      runGet (f x') end s'
  {-# INLINE (>>=) #-}
#if !(MIN_VERSION_base(4,13,0))
  fail = failGet
                 -- base < 4.13
#endif

#if MIN_VERSION_base(4,9,0)
instance Fail.MonadFail Get where
  fail = failGet
#endif
{-# INLINE failGet #-}
failGet :: String -> Get a
failGet msg = Get $ \end s -> badEncoding end s msg

-- |Decoder state
data S =
  S
    { currPtr  :: {-# UNPACK #-}!(Ptr Word8)
    , usedBits :: {-# UNPACK #-}!Int
    }
  deriving (Show, Eq, Ord)

data GetResult a =
  GetResult {-# UNPACK #-}!S !a
  deriving (Functor)

-- |A decoded value
type Decoded a = Either DecodeException a

-- |An exception during decoding
data DecodeException
  = NotEnoughSpace Env
  | TooMuchSpace Env
  | BadEncoding Env String
  deriving (Show, Eq, Ord)

type Env = (Ptr Word8, S)

notEnoughSpace :: Ptr Word8 -> S -> IO a
notEnoughSpace endPtr s = throwIO $ NotEnoughSpace (endPtr, s)

tooMuchSpace :: Ptr Word8 -> S -> IO a
tooMuchSpace endPtr s = throwIO $ TooMuchSpace (endPtr, s)

badEncoding :: Ptr Word8 -> S -> String -> IO a
badEncoding endPtr s msg = throwIO $ BadEncoding (endPtr, s) msg

instance Exception DecodeException

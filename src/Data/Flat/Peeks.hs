-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor ,BangPatterns ,ScopedTypeVariables ,MagicHash #-}
module Data.Flat.Peeks (
    Get,
    dBool,
    dWord8,
    dFloat,dDouble,
    -- dWord32be,
    --dWord64be,
    getChunksInfo,
    dByteString_,dLazyByteString_,dByteArray_,
    runGetStrict,runGetLazy,runGetRawLazy
    ) where

import        qualified    Data.ByteString as B
import qualified Data.ByteString.Lazy          as L
--import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BS
import           Data.Flat.Types
import           Data.Word
import           Foreign
import           System.IO.Unsafe
import           Control.Exception
-- -- -- -- -- -- -- import           Data.Bits
import           System.Endian
import Control.Monad
import           Data.Binary.FloatCast
import Data.Flat.Memory
-- import GHC.Types

--{-# INLINE unsafeChr #-}
--unsafeChr i@(I# i#) = C# (chr# i#)

{-# INLINE dBool #-}
dBool :: Get Bool
dBool = Get $ \endPtr s -> do
  if currPtr s >= endPtr
    then notEnoughSpace endPtr s
    else do
      !w <- peek (currPtr s)
      let !b = 0 /= (w .&. (128 `shiftR` usedBits s))
      -- let b = testBit w (7-usedBits s)
      let !s' = if usedBits s == 7
                then s {currPtr=currPtr s `plusPtr` 1,usedBits=0}
                else s {usedBits=usedBits s+1}
      return $ GetResult s' b

{-# INLINE dWord8  #-}
dWord8 :: Get Word8
dWord8 = Get $ \endPtr s -> do
      ensureBits endPtr s 8
      !w <- if usedBits s == 0
            then peek (currPtr s)
            else do
                   !w1 <- peek (currPtr s)
                   !w2 <- peek (currPtr s `plusPtr` 1)
                   return $ (w1 `unsafeShiftL` usedBits s) .|. (w2 `unsafeShiftR` (8-usedBits s))
      return $ GetResult (s {currPtr=currPtr s `plusPtr` 1}) w

{-# INLINE ensureBits #-}
ensureBits :: Ptr Word8 -> S -> Int -> IO ()
ensureBits endPtr s n = when ((endPtr `minusPtr` currPtr s) * 8 - usedBits s < n) $ notEnoughSpace endPtr s

{-# INLINE incBits #-}
incBits :: Int -> S -> S
incBits 1 s = if usedBits s == 7
           then s {currPtr=currPtr s `plusPtr` 1,usedBits=0}
           else s {usedBits=usedBits s+1}

incBits 8 s = s {currPtr=currPtr s `plusPtr` 1}

{-# INLINE dFloat #-}
dFloat :: Get Float
dFloat = Get $ \endPtr s -> do
  ensureBits endPtr s 32
  !w <- if usedBits s == 0
        then toBE32 <$> peek (castPtr $ currPtr s)
        else do
           !w1 <- toBE32 <$> peek (castPtr $ currPtr s)
           !(w2::Word8) <- peek (currPtr s `plusPtr` 4)
           return $ w1 `unsafeShiftL` usedBits s  .|. fromIntegral (w2 `unsafeShiftR` (8-usedBits s))
  return $ GetResult (s {currPtr=currPtr s `plusPtr` 4}) (wordToFloat w)

{-# INLINE dDouble #-}
dDouble :: Get Double
dDouble = Get $ \endPtr s -> do
  ensureBits endPtr s 64
  !w <- if usedBits s == 0
        then toBE64 <$> peek (castPtr $ currPtr s)
        else do
           !w1 <- toBE64 <$> peek (castPtr $ currPtr s)
           !(w2::Word8) <- peek (currPtr s `plusPtr` 8)
           return $ w1 `unsafeShiftL` usedBits s  .|. fromIntegral (w2 `unsafeShiftR` (8-usedBits s))
  return $ GetResult (s {currPtr=currPtr s `plusPtr` 8}) (wordToDouble w)

dLazyByteString_ :: Get L.ByteString
dLazyByteString_ = L.fromStrict <$> dByteString_

dByteString_ :: Get B.ByteString
dByteString_ = chunksToByteString <$> getChunksInfo

dByteArray_ :: Get (ByteArray,Int)
dByteArray_ = chunksToByteArray <$> getChunksInfo

getChunksInfo :: Get (Ptr Word8, [Int])
getChunksInfo = Get $ \endPtr s -> do

   let getChunks srcPtr l = do
          ensureBits endPtr s 8
          n <- fromIntegral <$> peek srcPtr
          if n==0
            then return (srcPtr `plusPtr` 1,l [])
            else do
              ensureBits endPtr s ((n+1)*8)
              getChunks (srcPtr `plusPtr` (n+1)) (l . (n:))

   when (usedBits s /=0) $ badEncoding endPtr s
   (currPtr',ns) <- getChunks (currPtr s) id
   return $ GetResult (s {currPtr=currPtr'}) (currPtr s `plusPtr` 1,ns)

runGetLazy :: Get a -> L.ByteString -> Either String a
runGetLazy get lbs = case runGetStrict get (L.toStrict lbs) of
  Left e -> Left (show e)
  Right r -> Right r

runGetRawLazy
  :: Exception e =>
     Get t -> L.ByteString -> Either e (t,B.ByteString, NumBits)
runGetRawLazy get lbs = runGetRawStrict get (L.toStrict lbs)

runGetStrict :: Get a -> B.ByteString -> Either GetException a
-- runGetStrict get bs = case runGetRawStrict get bs of
--                         Left e -> Left e
--                         Right (a,bs,o) ->
--                           if bs.length > 0 || o /= 0
--                           then 

runGetStrict get (BS.PS base off len) = unsafePerformIO . try $
    withForeignPtr base $ \base0 ->
        let ptr = base0 `plusPtr` off
            endPtr = ptr `plusPtr` len
        in do
          GetResult s'@(S ptr' o') a <- runGet get endPtr (S ptr 0)
          if ptr' /= endPtr || o' /= 0
            then tooMuchSpace endPtr s'
            else return a

runGetRawStrict
  :: Exception e =>
     Get t -> B.ByteString -> Either e (t,B.ByteString, NumBits)
-- runGetRawStrict get bs = case runGetRawStrict_ get bs off' of
--                            Left e -> Left e
--                            Right (GetResult (S ptr' o') a) ->
--                              if ptr' /= endPtr || o' /= 0
--                              then Right $ TooMuchSpace endPtr,s'
-- --             else return a

--                              if bs.length > 0 || o /= 0
--                              then 

runGetRawStrict get (BS.PS base off len) = unsafePerformIO . try $
    withForeignPtr base $ \base0 ->
        let ptr = base0 `plusPtr` off
            endPtr = ptr `plusPtr` len
        in do
          GetResult (S ptr' o') a <- runGet get endPtr (S ptr 0)
          return (a,BS.PS base (ptr' `minusPtr` base0) (endPtr `minusPtr` ptr'),o')

-- runGetRawStrict_
-- runGetRawStrict_ get (BS.PS base off len) = unsafePerformIO . try $
--     withForeignPtr base $ \base0 ->
--         let ptr = base0 `plusPtr` off
--             endPtr = ptr `plusPtr` len
--         in runGet get endPtr (S ptr 0)


newtype Get a = Get {runGet ::
                        Ptr Word8 -- End Ptr
                        -> S
                        -> IO (GetResult a)
                    } deriving Functor

instance Applicative Get where
    pure x = Get (\_ ptr -> return $ GetResult ptr x)
    {-# INLINE pure #-}

    Get f <*> Get g = Get $ \end ptr1 -> do
        GetResult ptr2 f' <- f end ptr1
        GetResult ptr3 g' <- g end ptr2
        return $ GetResult ptr3 (f' g')
    {-# INLINE (<*>) #-}

    Get f *> Get g = Get $ \end ptr1 -> do
        GetResult ptr2 _ <- f end ptr1
        g end ptr2
    {-# INLINE (*>) #-}

instance Monad Get where
    return = pure
    {-# INLINE return #-}

    (>>) = (*>)
    {-# INLINE (>>) #-}

    Get x >>= f = Get $ \end s -> do
        GetResult s' x' <- x end s
        runGet (f x') end s'
    {-# INLINE (>>=) #-}

    -- fail = getException
    -- {-# INLINE fail #-}

data S =
       S
         { currPtr  :: {-# UNPACK #-} !(Ptr Word8)
         , usedBits :: {-# UNPACK #-} !NumBits
         } deriving Show

data GetResult a = GetResult {-# UNPACK #-} !S !a deriving Functor
-- data GetResult a = GetResult !S !a deriving Functor

data GetException = NotEnoughSpace Env | TooMuchSpace Env | BadEncoding Env deriving Show

type Env = (Ptr Word8,S)

notEnoughSpace :: Ptr Word8 -> S -> IO a
notEnoughSpace endPtr s = throwIO $ NotEnoughSpace (endPtr,s)

tooMuchSpace :: Ptr Word8 -> S -> IO a
tooMuchSpace endPtr s = throwIO $ TooMuchSpace (endPtr,s)

badEncoding :: Ptr Word8 -> S -> IO a
badEncoding endPtr s = throwIO $ BadEncoding (endPtr,s)

instance Exception GetException



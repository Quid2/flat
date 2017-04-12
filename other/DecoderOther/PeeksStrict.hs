module Data.Flat.Peeks (
    Get,
    dBool,
    dWord8,
    dFloat,dDouble,
    -- dWord32be,
    --dWord64be,
    dByteString_,
    runGetStrict
    ) where

import           Data.ByteString
--import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BS
import           Data.Flat.Types
import           Data.Word
import           Foreign
import           System.IO.Unsafe
import           Control.Exception

{-# INLINE dBool #-}
dBool :: Get Bool
dBool = Get $ \endPtr s -> do
  if currPtr s == endPtr
    then notEnoughSpace endPtr s
    else do
      w :: Word8 <- peek (currPtr s)
      let b = testBit w (7-usedBits s)
      return $ if usedBits s  == 7
               then GetResult (s {currPtr=currPtr s `plusPtr` 1,usedBits=0}) b
               else GetResult (s {usedBits=usedBits s +1}) b

{-# INLINE dWord8  #-}
dWord8 :: Get Word8
dWord8 = undefined

{-# INLINE dFloat #-}
dFloat :: Get Float
dFloat = undefined

{-# INLINE dDouble #-}
dDouble :: Get Double
dDouble = undefined

-- {-# INLINE dWord32be  #-}
-- dWord32be :: Get Word32
-- dWord32be = getWord32be 32

-- {-# INLINE dWord64be  #-}
-- dWord64be :: Get Word64
-- dWord64be = getWord64be 64

{-# INLINE dByteString_  #-}
dByteString_ :: Int -> Get ByteString
dByteString_ = undefined

runGetStrict :: Get a -> ByteString -> Either GetException a
runGetStrict get (BS.PS base off len) = unsafePerformIO . try $
    withForeignPtr base $ \base0 ->
        let ptr = base0 `plusPtr` off
            endPtr = ptr `plusPtr` len
        in do
          GetResult (S ptr' o') a <- runGet get endPtr (S ptr 0)
          return $ if ptr' /= endPtr || o' /= 0
                   then tooMuchSpace endPtr S
                   else return a

newtype Get a = Get { runGet :: Ptr Word8 -> S -> IO (GetResult a) }

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

    Get x >>= f = Get $ \end ptr1 -> do
        GetResult ptr2 x' <- x end ptr1
        runGet (f x') end ptr2
    {-# INLINE (>>=) #-}

    fail = getException
    {-# INLINE fail #-}

-- Equivalent to a strict couple
data GetResult a = GetResult {-# UNPACK #-} !S !a

-- | Throws a 'GetException'.
getException :: String -> Get a
getException msg = Get $ \end ptr -> throwIO (GetException (end `minusPtr` ptr) msg)

data GetException = NotEnoughSpace Env | TooMuchSpace Env deriving Show

type Env = (Ptr Bool,S)

notEnoughSpace endPtr s = throwIO $ NotEnoughSpace (endPtr,s)
tooMuchSpace endPtr s = throwIO $ TooMuchSpace (endPtr,s)

instance Exception GetException

data S =
       S
         { currPtr  :: {-# UNPACK #-} !(Ptr Word8)
         , usedBits :: {-# UNPACK #-} !NumBits
         --, endPtr   :: {-# UNPACK #-} !(Ptr Word8)
         } deriving Show


type Encoder e = E -> e -> IO (Signal e)

data Signal e = Done {-# UNPACK #-} !S
              | NotEnoughSpace {-# UNPACK #-} !S {-# UNPACK #-} !Int e
              | InsertByteString {-# UNPACK #-} !S BS.ByteString e
  deriving Show

done :: forall e m. Monad m => S -> m (Signal e)
done = return . Done

notEnoughSpace :: Monad m => S -> Int -> e -> m (Signal e)
notEnoughSpace s n e = return (NotEnoughSpace s n e)

getState :: Signal t -> S
getState (Done s)               = s
getState (NotEnoughSpace s _ _) = s
getState (InsertByteString s _ _) = s
--data E = E { lastPtr :: {-# UNPACK #-} !(Ptr Word8), currState :: {-# UNPACK #-} !S }

data E = E { lastPtr :: {-# UNPACK #-} !(Ptr Word8), currState :: !S }

{-# INLINE availBytes #-}
availBytes :: E -> Int
availBytes e = lastPtr e `minusPtr` nextPtr (currState e)

{-# INLINE availBits #-}
availBits :: E -> Int
-- We consider only the memory space
availBits = (8*) . availBytes
--availBits e = 8* (lastPtr e `minusPtr` nextPtr (currState e)) - usedBits (currState e)
-- availBits (E lastPtr s) = 8* (lastPtr `minusPtr` nextPtr s) - usedBits s

--{-# INLINE availBits_ #-}
--availBits_ :: Ptr Word8 -> S -> Int
--availBits_ lastPtr s = 8* (lastPtr `minusPtr` nextPtr s) - usedBits s


{-# INLINE hasBytes #-}
hasBytes :: E -> Int -> Bool
hasBytes e n = nextPtr (currState e) `plusPtr` n <= lastPtr e

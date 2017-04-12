{-# NOINLINE historicSizes #-}
-- Store observed sizes of encodings
historicSizes :: TVar Int
-- historicSizes :: TVar (M.Map TypeRep Int)
-- historicSizes = unsafePerformIO $ newTVarIO M.empty
historicSizes = unsafePerformIO $ newTVarIO 1

-- Try to encode with a guessed size, if it fails restart enconding from start with a longer size
bitEncoderRetry :: Show e => Encoder e -> e -> L.ByteString
bitEncoderRetry enc e = unsafePerformIO $ do
  guessedSize <- atomically $ readTVar historicSizes
  let bs = bitEncoderRetry_ guessedSize enc e
  let lastSize = fromIntegral $ L.length bs
  atomically $ writeTVar historicSizes lastSize
  -- print $ unwords ["guessedSize",show guessedSize,"actualSize",show lastSize,if lastSize > guessedSize then "MISS" else "HIT"]
  return bs

bitEncoderRetry_ :: Show e => Int -> Encoder e -> e -> L.ByteString
bitEncoderRetry_ initialBufSize encoder encoding | initialBufSize > 0 = enc [] initialBufSize 0 0 encoding
                                                 | otherwise = error "bitEncoderRetry_: initialBuffer size must be positive"
  where
   enc bufs bufSize w u e = do
     let (bs,signal) = unsafeCreateUptoN' bufSize $ \ptr -> do
           --print $ unwords ["bufsize",show bufSize]
           !s <- encoder (E (ptr `plusPtr` bufSize) (S ptr w u)) e
           return (nextPtr (getState s) `minusPtr` ptr,s)

     case traceShowId signal of
       Done s' | currByte s' == 0 && usedBits s' == 0 -> L.fromChunks . reverse $ bs:bufs
       -- Retry n -> bitEncoderRetry (nextBufSize bufSize (bitsToBytes n)) encoder encoding
       -- NotEnoughSpace _ n _ -> bitEncoderRetry (nextBufSize bufSize (bitsToBytes n)) encoder encoding
       NotEnoughSpace _ _ _ -> enc [] (bufSize*2) 0 0 encoding
       -- BUG: wasted space, also state cannot be returned so usedBits must be == 0
       --InsertBytes ibs e' -> enc (ibs:bs:bufs) (nextBufSize initialBufSize bufSize) 0 0 e'
       o -> error $ unwords ["bitEncoderRetry: Unexpected value",show o]

-- Note: wastes some space
-- TODO: avoid empty or very short chunks
bitEncoderLazy :: Show e => Int -> Encoder e -> e -> L.ByteString
bitEncoderLazy initialBufSize encoder encoding | initialBufSize > 0 = enc [] initialBufSize 0 0 encoding
                                               | otherwise = error "bitEncoderLazy: initialBuffer size must be positive"
  where
   enc bufs bufSize w u e = do
     let (bs,signal) = unsafeCreateUptoN' bufSize $ \ptr -> do
           --print $ unwords ["bufsize",show bufSize]
           !s <- encoder (E (ptr `plusPtr` bufSize) (S ptr w u)) e
           return (nextPtr (getState s) `minusPtr` ptr,s)

     case traceShowId signal of
       Done s' | currByte s' == 0 && usedBits s' == 0 -> L.fromChunks . reverse $ bs:bufs
       NotEnoughSpace s' n e' -> enc (bs:bufs) (nextBufSize bufSize (bitsToBytes n)) (currByte s') (usedBits s') e'
       -- BUG: wasted space, also state cannot be returned so usedBits must be == 0
       -- InsertBytes ibs e' -> enc (ibs:bs:bufs) (nextBufSize initialBufSize bufSize) 0 0 e'
       o -> error $ unwords ["bitEncoderLazy: Unexpected value",show o]

nextBufSize :: Integral a => a -> a -> a
nextBufSize currentSize requestedSize = max requestedSize $ if currentSize < 65536 then currentSize *2 else currentSize * 3 `div` 2


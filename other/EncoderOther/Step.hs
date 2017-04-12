data Step = Step { stepSize :: !NumBits, stepF :: Prim }

instance Show Step where show (Step n _) = unwords ["Step",show n]

{-# INLINE eWord64S #-}
eWord64S :: Word64 -> Step
eWord64S t = Step (10*8) (varWordF t)

{-# INLINE eWord32S #-}
eWord32S :: Word32 -> Step
eWord32S t = Step (5*8) (varWordF t)

{-# INLINE eWord16S #-}
eWord16S :: Word16 -> Step
eWord16S t = Step (3*8) (varWordF t)

{-# INLINE eWord64LE #-}
eWord64LE :: Word64 -> Step
eWord64LE t = Step 64 (eWord64E toLE64 t)

{-# INLINE eWord32LE #-}
eWord32LE :: Word32 -> Step
eWord32LE t = Step 32 (eWord32E toLE32 t)

{-# INLINE eWord64BES #-}
eWord64BES :: Word64 -> Step
eWord64BES t = Step 64 (eWord64BEF t)

{-# INLINE eWord32BES #-}
eWord32BES :: Word32 -> Step
eWord32BES t = Step 32 (eWord32BEF t)

{-# INLINE eBitsS #-}
eBitsS :: NumBits -> Word8 -> Step
eBitsS n t = Step n $ eBitsF n t

{-# INLINE eFillerS #-}
eFillerS :: Step
eFillerS = Step 8 eFillerF

{-# INLINE eBoolS #-}
eBoolS :: Bool -> Step
eBoolS = Step 1 . eBoolF

{-# INLINE eTrueS #-}
eTrueS :: Step
eTrueS = Step 1 eTrueF

{-# INLINE eFalseS #-}
eFalseS :: Step
eFalseS = Step 1 eFalseF

eIntegerS :: Integer -> Step
eIntegerS = eIntegralS . zzEncodeInteger

{-# INLINE eWord8S #-}
eWord8S :: Word8 -> Step
eWord8S t = Step 8 (eWord8F t)


eNaturalS :: Natural -> Step
eNaturalS = eIntegralS . toInteger

{-# INLINE eIntegralS #-}
eIntegralS :: Integer -> Step
eIntegralS t = let vs = w7l $ t
               in Step (length vs*8) (eIntegralW vs)

eUTF16S :: T.Text -> Step
eUTF16S !t = Step (sUTF16 t) (eUTF16F t)

eBytesS :: B.ByteString -> Step
eBytesS bs = Step (sBytes bs) (eBytesF bs)

eLazyBytesS :: L.ByteString -> Step
eLazyBytesS bs = Step (sLazyBytes bs) (eLazyBytesF bs)

{-# INLINE eShortBytesS #-}
eShortBytesS :: SBS.ShortByteString -> Step
eShortBytesS bs = Step (sShortBytes bs) (eShortBytesF bs)

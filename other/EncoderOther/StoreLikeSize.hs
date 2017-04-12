-- Store like Sizes
-- actual sizes
-- sChar = VarSize P.sCharV
-- sWord = VarSize P.sWordV
-- sInt = VarSize P.sIntV
-- sWord64 = VarSize P.sWord64V
-- sWord32 = VarSize P.sWord32V
-- sWord16 = VarSize P.sWord16V
-- sInt64 = VarSize P.sInt64V
-- sInt32 = VarSize P.sInt32V
-- sInt16 = VarSize P.sInt16V

-- -- Fixed sizes
-- sChar = ConstSize P.sChar
-- sInt64 = ConstSize P.sInt64
-- sInt32 = ConstSize P.sInt32
-- sInt16 = ConstSize P.sInt16
-- sInt8 = ConstSize P.sInt8
-- sInt = ConstSize P.sInt
-- sWord64 = ConstSize P.sWord64
-- sWord32 = ConstSize P.sWord32
-- sWord16 = ConstSize P.sWord16
-- sWord8 = ConstSize P.sWord8
-- sWord = ConstSize P.sWord

-- sBytes = VarSize P.sBytes
-- sLazyBytes = VarSize P.sLazyBytes
-- sShortBytes = VarSize P.sShortBytes
-- sNatural = VarSize P.sNatural
-- sInteger = VarSize P.sInteger
-- sUTF16 = VarSize P.sUTF16

-- sInt8 = ConstSize P.sInt8
-- sWord8 = ConstSize P.sWord8
-- sFloat = ConstSize P.sFloat
-- sDouble = ConstSize P.sDouble
-- sFiller = ConstSize P.sFiller
-- sBool = ConstSize P.sBool


{-# INLINE size #-}
    -- size :: Size a
    -- default size :: (Generic a, GSize (Rep a)) => Size a
    -- size = genericSize

-- Store-Like Size
-- genericSize :: (Generic a, GSize (Rep a)) => Size a
-- genericSize = contramapSize from gsize


-- -- | Info about a type's serialized length. Either the length is known
-- -- independently of the value, or the length depends on the value.
-- data Size a
--     = VarSize (a -> Int)
--     | ConstSize !Int
--     -- deriving Typeable

-- instance Show (Size a) where
--   show (ConstSize n) = unwords ["ConstSize",show n]
--   show (VarSize _) = "VarSize"

-- -- | Given a 'Size' value and a value of the type @a@, returns its 'Int'
-- -- size.
-- getSizeWith :: Size a -> a -> Int
-- getSizeWith (VarSize f) x = f x
-- getSizeWith (ConstSize n) _ = n
-- {-# INLINE getSizeWith #-}

-- -- | This allows for changing the type used as an input when the 'Size'
-- -- is 'VarSize'.
-- contramapSize :: (a -> b) -> Size b -> Size a
-- contramapSize f (VarSize g) = VarSize (g . f)
-- contramapSize _ (ConstSize n) = ConstSize n
-- {-# INLINE contramapSize #-}

-- -- | Create an aggregate 'Size' by providing functions to split the
-- -- input into two pieces, as well as 'Size' values to use to measure the
-- -- results.
-- --
-- -- If both of the input 'Size' values are 'ConstSize', the result is
-- -- 'ConstSize' and the functions will not be used.
-- combineSizeWith :: forall a b c. (c -> a) -> (c -> b) -> Size a -> Size b -> Size c
-- combineSizeWith toA toB sizeA sizeB =
--     case (sizeA, sizeB) of
--         (VarSize f, VarSize g) -> VarSize (\x -> f (toA x) + g (toB x))
--         (VarSize f, ConstSize m) -> VarSize (\x -> f (toA x) + m)
--         (ConstSize n, VarSize g) -> VarSize (\x -> n + g (toB x))
--         (ConstSize n, ConstSize m) -> ConstSize (n + m)
-- {-# INLINE combineSizeWith #-}

-- -- | Adds a constant amount to a 'Size' value.
-- addSize :: Int -> Size a -> Size a
-- addSize x (ConstSize n) = ConstSize (x + n)
-- addSize x (VarSize f) = VarSize ((x +) . f)
-- {-# INLINE addSize #-}



-- -- | Get the number of bytes needed to store the given value. See
-- -- 'size'.
-- getSize :: Flat a => a -> Int
-- getSize = getSizeWith size
-- {-# INLINE getSize #-}

-- cmapSize :: forall a b. Flat b => (a -> b) -> Size a
-- cmapSize f = case size :: Size b of
--                VarSize g -> VarSize (g . f)
--                ConstSize n -> ConstSize n
-- {-# INLINE cmapSize #-}

-- combineSizes :: forall a b c. (Flat a, Flat b) => (c -> a) -> (c -> b) -> Size c
-- combineSizes toA toB =
--     case (size::Size a, size::Size b) of
--         (VarSize f, VarSize g) -> VarSize (\x -> f (toA x) + g (toB x))
--         (VarSize f, ConstSize m) -> VarSize (\x -> f (toA x) + m)
--         (ConstSize n, VarSize g) -> VarSize (\x -> n + g (toB x))
--         (ConstSize n, ConstSize m) -> ConstSize (n + m)
-- {-# INLINE combineSizes #-}

-- class GSize f where gsize :: Size (f a)

-- instance GSize f => GSize (M1 i c f) where
--     gsize = contramapSize unM1 gsize
--     {-# INLINE gsize #-}

-- -- Type without constructors
-- instance GSize V1 where
--     gsize = ConstSize 0
--     {-# INLINE gsize #-}

-- -- Constructor without arguments
-- instance GSize U1 where
--     gsize = ConstSize 0
--     {-# INLINE gsize #-}
 
-- instance Flat a => GSize (K1 i a) where
--     gsize = contramapSize unK1 size
--     {-# INLINE gsize #-}

-- instance (GSize a, GSize b) => GSize (a :*: b) where
--     gsize = combineSizeWith (\(x :*: _) -> x) (\(_ :*: y) -> y) gsize gsize
--     {-# INLINE gsize #-}

-- instance GSizeSum 0 (a :+: b) => GSize (a :+: b) where
--     gsize = VarSize $ \x -> gsizeSum x (Proxy :: Proxy 0)
--     {-# INLINE gsize #-}

-- class KnownNat n => GSizeSum (n :: Nat) (f :: * -> *) where gsizeSum :: f a -> Proxy n -> NumBits

-- instance (GSizeSum (n+1) a, GSizeSum (n+1) b, KnownNat n) => GSizeSum n (a :+: b) where
--     gsizeSum !(L1 !l) _ = gsizeSum l (Proxy :: Proxy (n+1))
--     gsizeSum !(R1 !r) _ = gsizeSum r (Proxy :: Proxy (n+1))
--     {-# INLINE gsizeSum #-}

-- instance (GSize a, KnownNat n) => GSizeSum n (C1 c a) where
--     gsizeSum !x _ = getSizeWith (addSize constructorSize gsize) x
--       where
--         constructorSize :: Int
--         constructorSize = fromInteger (natVal (Proxy :: Proxy n))
--     {-# INLINE gsizeSum #-}


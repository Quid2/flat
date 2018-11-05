{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}
-- | Primitives to convert between Float/Double and Word32/Word64
-- | This code was copied from `binary`
-- | This module was written based on
--   <http://hackage.haskell.org/package/reinterpret-cast-0.1.0/docs/src/Data-ReinterpretCast-Internal-ImplArray.html>.
--
--   Implements casting via a 1-element STUArray, as described in
--   <http://stackoverflow.com/a/7002812/263061>.
module Data.FloatCast
  ( floatToWord
  , wordToFloat
  , doubleToWord
  , wordToDouble
  )
where

import           Data.Word                      ( Word32
                                                , Word64
                                                )
import           Data.Array.ST                  ( newArray
                                                , readArray
                                                , MArray
                                                , STUArray
                                                )
import           Data.Array.Unsafe              ( castSTUArray )
import           GHC.ST                         ( runST
                                                , ST
                                                )
import           Data.Flat.Endian

-- | Reinterpret-casts a `Float` to a `Word32`.
floatToWord :: Float -> Word32
floatToWord x = runST (cast x)
{-# INLINE floatToWord #-}

-- | Reinterpret-casts a `Double` to a `Word64`.
{-|
>>> doubleToWord (-0.15625)
13818169556679524352
-}
doubleToWord :: Double -> Word64
doubleToWord x = fix64 $ runST (cast x)

-- #ifdef ghcjs_HOST_OS
-- doubleToWord x = (`rotateR` 32) $ runST (cast x)
-- #else
-- doubleToWord x = runST (cast x)
-- #endif

{-# INLINE doubleToWord #-}

-- | Reinterpret-casts a `Word32` to a `Float`.
wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)
{-# INLINE wordToFloat #-}

-- | Reinterpret-casts a `Word64` to a `Double`.
{-# INLINE wordToDouble #-}
wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast $ fix64 x)

-- #ifdef ghcjs_HOST_OS
-- wordToDouble x = runST (cast $ x `rotateR` 32) 
-- #else
-- wordToDouble x = runST (cast x) 
-- #endif

cast
  :: (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
{-# INLINE cast #-}

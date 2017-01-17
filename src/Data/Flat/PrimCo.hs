{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Data.Flat.Prim(Encoding,(<>),mempty,bitEncoder,eBits,eFiller,eBool,eTrue,eFalse,eWord8,eWord32,eWord64,eUnsigned,eLazyBytes,eBytes) where

import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy     as L
import           Data.Flat.Pokes          hiding (eBitsF, eBoolF, eFalseF,
                                           eTrueF, pokeWord)
import           Data.Foldable
import           Data.Monoid
import           Data.Word
import           Foreign
import           Foreign.Ptr
import           System.IO.Unsafe

c = bitEncoder $ eBits 2 3 <> eTrue <> eTrue <> eFiller

newtype Encoding = Encoding {enc:: E -> IO (Signal Encoding)}

instance Show Encoding where show _ = "Encoding"

instance Monoid Encoding where
  mempty = Encoding (done . currState)

  mappend (Encoding f) g = Encoding $ \e@(E fp _) -> do
    ecs <- f e
    case ecs of
      Done s'               -> enc g (E fp s')
      NotEnoughSpace n s' k -> return (NotEnoughSpace n s' (k <> g))

bitEncoder = bitEncoderLazy 4096 (\e op -> (enc op) e)

{-# INLINE eBits #-}
eBits :: Int -> Word8 -> Encoding
eBits n t = Encoding $ \(E fp (S op w o)) ->
  let o' = o + n  -- used bits
      f = 8 - o'  -- remaining free bits
  in if | f > 0  ->  done $ S op (w .|. (t `shiftL` f)) o'
        | f == 0 ->  pokeWord fp op (w .|. (t `shiftL` f))
        | otherwise -> chkPoke (t `shiftL` (8 - o')) (-f) fp op (w .|. (t `shiftR` o'))

eTrue,eFalse,eFiller::Encoding

{-# INLINE eBool #-}
eBool False = eFalse
eBool True  = eTrue

{-# INLINE eTrue #-}
eTrue = Encoding eTrueF

{-# INLINE eFalse #-}
eFalse = Encoding eFalseF

{-# INLINE eFalseF #-}
eFalseF (E fp (S op w o)) | o == 7 = pokeWord fp op w
                          | otherwise = done (S op w (o+1))

{-# INLINE eTrueF #-}
eTrueF (E fp (S op w o)) | o == 7 = pokeWord fp op (w .|. 1)
                         | otherwise = done (S op (setBit w (7-o)) (o+1))

{-# INLINE eFiller #-}
eFiller = Encoding $ \(E fp (S op w _)) -> pokeWord fp op (w .|. 1)

{-# INLINE pokeWord #-}
pokeWord = chkPoke 0 0

{-# INLINE chkPoke #-}
chkPoke w' n' fp op w
  | op < fp = poke op w >> done (S (plusPtr op 1) w' n')
  | otherwise = notEnoughSpace (S op w' n') 1
                  (Encoding $ \(E _ s) -> poke (nextPtr s) w >> done (s { nextPtr = plusPtr (nextPtr s) 1 }))

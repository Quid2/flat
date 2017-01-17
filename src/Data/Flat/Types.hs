module Data.Flat.Types(Encoding
                      ,Op(..)
                      ,Word8
                      ,module Data.Seq
                      ,(<>)) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Monoid
import           Data.Seq
import           Data.Word

-- |An encoding is a sequence of encoding Ops
-- Values are first converted to a sequence of Ops and then converted to binary
-- This approach has been introduced by Duncan Coutts in his binary-serialise-cbor package
type Encoding = Seq Op

-- |Encoding operation
data Op =
        -- |Encode up to 8 bits (most significant bit first)
         Tag8 { numBitsToEncode :: {-# UNPACK #-} !Int, bitsToEncode :: {-# UNPACK #-} !Word8 }
        |
        -- |Encode a single bit
         TagBool !Bool
        |
        -- |Encode an unsigned Word of length up to 64 bits as a non-empty list of 7 bits Words as
        -- in the data type: data List = Elem Word7 | Cons Word7 List Used by all Word and Int
        -- encodings
         TagWord64 {-# UNPACK #-} !Word64
        |
        -- |Encode as many bits as required to get to the next byte boundary If the next bit to
        -- fill is the first of an additional byte, this will add an additional byte
         TagByteFiller
        |
        -- |Encode a sequence of bytes
        TagBytes {-# UNPACK #-} !B.ByteString

        | TagLazyBytes !L.ByteString
        -- forall a s. Sequence s => TagList (a -> s Op) [a] | TagList [Op] | TagText {-# UNPACK #-} !T.Text
        deriving (Show, Eq)

-- encodingMaxLength = \case
--   (Tag8 _) -> 1
--   (Tag)

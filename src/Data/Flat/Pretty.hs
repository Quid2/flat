module Data.Flat.Pretty(Pretty(..),prettyShow,prettyWord8) where
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import           Data.Word
import           Text.PrettyPrint.HughesPJClass
import           Text.Printf

instance Pretty B.ByteString where pPrint = prettyBL . B.unpack

instance Pretty L.ByteString where pPrint = prettyBL . L.unpack

-- prettyLBS :: L.ByteString -> String
-- prettyLBS = text . unwords . map prettyWord8 . L.unpack

prettyBL = text . unwords . map prettyWord8

instance Pretty Word8 where pPrint = text . show

prettyWord8 :: Word8 -> String
prettyWord8 = printf "%08b"

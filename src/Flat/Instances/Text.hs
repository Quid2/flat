{-# LANGUAGE CPP #-}

-- | Flat instances for the text library
module Flat.Instances.Text(
  UTF8Text(..)
#if! defined(ghcjs_HOST_OS) && ! defined (ETA_VERSION) && ! defined (ETA) 
  ,UTF16LEText(..)
#endif
) where

import           Flat.Instances.Util
import qualified Data.Text             as T
import qualified Data.Text.Lazy             as TL

-- $setup
-- >>> import Flat.Instances.Base()
-- >>> import Flat.Instances.Test
-- >>> import qualified Data.Text             as T
-- >>> import qualified Data.Text.Lazy             as TL
-- >>> import Data.Word    

{-|
Text (and Data.Text.Lazy) is encoded as a byte aligned array of bytes corresponding to its UTF8 encoding.

>>> tst $ T.pack ""
(True,16,[1,0])

>>> tst $ T.pack "aaa"
(True,120,[1,3,97,97,97,0])

>>> tst $ T.pack "¢¢¢"
(True,120,[1,6,194,162,194,162,194,162,0])

>>> tst $ T.pack "日日日"
(True,120,[1,9,230,151,165,230,151,165,230,151,165,0])

#ifndef ETA
>>> tst $ T.pack "𐍈𐍈𐍈"
(True,120,[1,12,240,144,141,136,240,144,141,136,240,144,141,136,0])
#endif

Strict and Lazy Text has the same encoding:

>>> tst (T.pack "abc") == tst (TL.pack "abc")
True
-}
instance Flat T.Text where
  size = sUTF8Max
  encode = eUTF8
  decode = dUTF8

instance Flat TL.Text where
  size = sUTF8Max . TL.toStrict
  encode = eUTF8 . TL.toStrict
  decode = TL.fromStrict <$> dUTF8

{-|
The desired text encoding can be explicitly specified using the wrappers UTF8Text and UTF16LEText.

The default encoding is UTF8:

>>> tst (UTF8Text $ T.pack "日日日") == tst (T.pack "日日日")
True
-}
-- |A wrapper to encode/decode Text as UTF8 (slower but more compact)
newtype UTF8Text = UTF8Text {unUTF8::T.Text} deriving (Eq,Ord,Show)

instance Flat UTF8Text where
  size (UTF8Text t) = sUTF8Max t
  encode (UTF8Text t) = eUTF8 t
  decode = UTF8Text <$> dUTF8

#if ! defined(ghcjs_HOST_OS) && ! defined (ETA_VERSION) && ! defined (ETA) 
{-|
>>> tst (UTF16LEText $ T.pack "aaa")
(True,72,[1,6,97,0,97,0,97,0,0])

>>> tst (UTF16LEText $ T.pack "𐍈𐍈𐍈")
(True,120,[1,12,0,216,72,223,0,216,72,223,0,216,72,223,0])
-}

-- |A wrapper to encode/decode Text as UTF16 (faster but bigger)
newtype UTF16LEText = UTF16LEText {unUTF16::T.Text} deriving (Eq,Ord,Show)

instance Flat UTF16LEText where
  size (UTF16LEText t) = sUTF16 t
  encode (UTF16LEText t) = eUTF16 t
  decode = UTF16LEText <$> dUTF16

#endif


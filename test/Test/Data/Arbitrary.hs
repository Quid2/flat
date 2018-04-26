{-# LANGUAGE CPP #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Test.Data.Arbitrary where

-- import qualified Data.ByteString           as B
-- import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString.Short     as SBS
-- import qualified Data.Text                 as T
-- import           Test.Data
-- import           Data.DeriveTH
import           Test.QuickCheck.Instances
import           Test.Tasty.QuickCheck
-- import           Test.QuickCheck

-- xxx = generate (arbitrary :: Gen (Large (Int)))

#if !MIN_VERSION_quickcheck_instances(0,3,17)
instance Arbitrary SBS.ShortByteString where arbitrary   = fmap SBS.pack arbitrary
#endif


{-
-- derive makeArbitrary ''N
derive makeArbitrary ''Tree

derive makeArbitrary ''List

derive makeArbitrary ''Unit

derive makeArbitrary ''Un

derive makeArbitrary ''A

derive makeArbitrary ''B
-}

-- instance Arbitrary Word7 where arbitrary  = toEnum <$> choose (0, 127)
-- derive makeArbitrary ''ASCII

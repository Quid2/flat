{-# LANGUAGE StandaloneDeriving ,DeriveAnyClass , CPP #-}
module Test.E.Flat where

import           Test.E
import           Data.Flat

deriving instance Flat E2
deriving instance Flat E3
deriving instance Flat E4
deriving instance Flat E8
deriving instance Flat E16
deriving instance Flat E17
deriving instance Flat E32

#ifdef ENUM_LARGE
deriving instance Flat E256
deriving instance Flat E258
#endif

-- fs =
--     [ flat E2_1,flat E3_1
--     , flat E4_1
--     , flat E8_1
--     , flat E16_1
--     , flat E32_1
--     , flat E256_255
--     , flat E256_254
--     , flat E256_253
--     , flat E256_256
--     ]



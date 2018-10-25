{-# LANGUAGE MultiParamTypeClasses ,DeriveGeneric ,DeriveDataTypeable ,ScopedTypeVariables ,GADTs ,NoMonomorphismRestriction ,DeriveGeneric ,DefaultSignatures ,TemplateHaskell ,TypeFamilies ,FlexibleContexts ,FlexibleInstances ,EmptyDataDecls #-}
module Test.Data2 where

import           Data.Typeable
import           Data.Data
import           GHC.Generics

-- A definition with the same name of a definition in Test.Data, used to test for name clashes.a
data List a = Cons2 a (List a)
            | Nil2
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic ,Generic1)


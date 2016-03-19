{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses ,DeriveGeneric ,DeriveDataTypeable ,ScopedTypeVariables ,GADTs ,NoMonomorphismRestriction ,DeriveGeneric ,DefaultSignatures ,TemplateHaskell ,TypeFamilies ,FlexibleContexts ,FlexibleInstances ,EmptyDataDecls #-}
{-
 A collection of data types used for testing.
-}
module Test.Data where

import Control.Exception
import           Data.Char
import           Data.Int
import           Data.Word

import           Data.Typeable
import           Data.Data
import           GHC.Generics

import qualified Test.Data2 as D2
import Data.Foldable
import GHC.Exts hiding (toList)

data Void deriving Generic

data X = X X deriving Generic

data Unit = Unit deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Un = Un {un::Bool} deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data D2 = D2 Bool N deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data D4 = D4 Bool N Unit N3 deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- Enumeration
data N3 = N1 | N2 | N3
            deriving (Eq, Ord, Read, Show, Typeable, Data, Generic,Enum)

data N = One
       | Two
       | Three
       | Four
       | Five
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, Enum)

data Forest a = Forest (List (Tr a)) deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Tr a = Tr a (Forest a) deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Words = Words Word8 Word16 Word32 Word64
            deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Ints = Ints Int8 Int16 Int32 Int64
            deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- non-recursive data type
data Various = V1 (Maybe Char) (Maybe Word8) | V2 () (Either Word8 Int)
              deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- Phantom type
data Phantom a = Phantom deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)


-- Recursive data types

data RR a b c = RN {rna::a, rnb::b ,rnc::c}
              | RA a (RR a a c) b
              | RAB a (RR c b a) b
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Expr = ValB Bool | Or Expr Expr | If Expr Expr Expr  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data List a = C a (List a)
            | N
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic ,Generic1,Functor,Foldable)

data ListS a = Nil | Cons a (ListS a)
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic ,Generic1)

-- non-regular Haskell datatypes like:
-- Binary instances but no HasModel
data Nest a = NilN | ConsN (a, Nest (a, a)) deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data TN a = LeafT a | BranchT (TN (a,a)) deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Bush a = NilB | ConsB (a, Bush (Bush a)) deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
-- Perfectly balanced binary tree
data Perfect a = ZeroP a | SuccP (Perfect (Fork a)) deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
data Fork a = Fork a a deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- non regular with higher-order kind parameters
-- no Binary/HasModel instances 
data PerfectF f α = NilP | ConsP α (PerfectF f (f α)) deriving (Typeable,Generic) -- No Data

data Pr f g a = Pr (f a (g a))

-- data Pr2 (f :: * -> *) a = Pr2 (f )

data Free f a = Pure a | Roll (f (Free f a)) deriving (Typeable,Generic)

-- mutual references
data A = A B | AA Int deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
data B = B A | BB Char deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)


-- recursive sets:
-- Prob: ghc will just explode on this
-- data MM1 = MM1 MM2 MM4 MM0 deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
-- data MM0 = MM0 deriving (Eq, Ord, Read, Show, Typeable, Data, Generic) 
-- data MM2 = MM2 MM3 Bool deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
-- data MM3 = MM3 MM4 Bool deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
-- data MM4 = MM4 MM4 MM2 MM5 deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
-- data MM5 = MM5 Unit MM6 deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
-- data MM6 = MM6 MM5 deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data A0 = A0 B0 B0 D0 Bool
        | A1 (List Bool) (List Unit) (D2.List Bool) (D2.List Bool)
        deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
data B0 = B0 C0 | B1 deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
data C0 = C0 A0 | C1 deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
data D0 = D0 E0 | D1 deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
data E0 = E0 D0 | E1 deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Even = Zero | SuccE Odd
data Odd = SuccO Even

-- Existential types
-- data Fold a b = forall x. Fold (x -> a -> x) x (x -> b)

-- data Some :: (* -> *) -> * where
--   Some :: f a -> Some f

-- data Dict (c :: Constraint) where
--   Dict :: c => Dict c

data Stream a = Stream a (Stream a)
            deriving (Eq, Ord, Read, Show, Typeable, Data, Generic,Functor,Foldable,Traversable)

data Tree a = Node (Tree a) (Tree a) | Leaf a
            deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- Example schema from: http://mechanical-sympathy.blogspot.co.uk/2014/05/simple-binary-encoding.html
data Car = Car {
  serialNumber::Word64
  ,modelYear::Word16
  ,available::Bool
  ,code::CarModel
  ,someNumbers::[Int32]
  ,vehicleCode::String
  ,extras::[OptionalExtra]
  ,engine::Engine
  ,fuelFigures::[Consumption]
  ,performanceFigures :: [(OctaneRating,[Acceleration])]
  ,make::String
  ,carModel::String
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Acceleration = Acceleration {mph::Word16,seconds::Float} deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

type OctaneRating = Word8 -- minValue="90" maxValue="110"

data Consumption = Consumption {cSpeed::Word16,cMpg::Float} deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data CarModel = ModelA | ModelB | ModelC  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data OptionalExtra = SunRoof | SportsPack | CruiseControl deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Engine = Engine {
  capacity :: Word16
  ,numCylinders:: Word8
  ,maxRpm:: Word16 -- constant 9000
  ,manufacturerCode :: String
  ,fuel::String -- constant Petrol
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)


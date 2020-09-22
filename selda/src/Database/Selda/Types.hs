{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE GADTs, TypeOperators, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | Basic Selda types.
module Database.Selda.Types
  ( (:*:)(..), Head, Tup (..)
  , first, second, third, fourth, fifth
  , ColName, TableName
  , modColName, mkColName, mkTableName, addColSuffix, addColPrefix
  , fromColName, fromTableName, rawTableName, intercalateColNames
  ) where
import Data.Dynamic
import Data.String
import Data.Text (Text, replace, append, intercalate)
import GHC.Generics (Generic)
import Database.Selda.Core.Types


-- | An inductively defined "tuple", or heterogeneous, non-empty list.
data a :*: b where
  (:*:) :: a -> b -> a :*: b
  deriving (Typeable, Generic)
infixr 1 :*:

instance (Show a, Show b) => Show (a :*: b) where
  show (a :*: b) = show a ++ " :*: " ++ show b

instance (Eq a, Eq b) => Eq (a :*: b) where
  (a :*: b) == (a' :*: b') = a == a' && b == b'

instance (Ord a, Ord b) => Ord (a :*: b) where
  (a :*: b) `compare` (a' :*: b') =
    case a `compare` a' of
      EQ -> b `compare` b'
      o  -> o

type family Head a where
  Head (a :*: b) = a
  Head a         = a

class Tup a where
  tupHead :: a -> Head a

instance {-# OVERLAPPING #-} Tup (a :*: b) where
  tupHead (a :*: _) = a

instance Head a ~ a => Tup a where
  tupHead a = a

-- | Get the first element of an inductive tuple.
first :: Tup a => a -> Head a
first = tupHead

-- | Get the second element of an inductive tuple.
second :: Tup b => (a :*: b) -> Head b
second (_ :*: b) = tupHead b

-- | Get the third element of an inductive tuple.
third :: Tup c => (a :*: b :*: c) -> Head c
third (_ :*: _ :*: c) = tupHead c

-- | Get the fourth element of an inductive tuple.
fourth :: Tup d => (a :*: b :*: c :*: d) -> Head d
fourth (_ :*: _ :*: _ :*: d) = tupHead d

-- | Get the fifth element of an inductive tuple.
fifth :: Tup e => (a :*: b :*: c :*: d :*: e) -> Head e
fifth (_ :*: _ :*: _ :*: _ :*: e) = tupHead e

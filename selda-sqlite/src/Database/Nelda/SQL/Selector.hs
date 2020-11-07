{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SQL.Selector where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Database.Nelda.SQL.Col (Col (One))
import Database.Nelda.SQL.Nullability
import Database.Nelda.SQL.Row (C, CS, Row (Many), (:-))
import Database.Nelda.SQL.Types (UntypedCol (Untyped))
import Database.Nelda.SqlType (SqlType)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownNat, Nat, Symbol, natVal, type (+))
import Unsafe.Coerce (unsafeCoerce)

-- | A column selector. Column selectors can be used together with the '!' and
--   'with' functions to get and set values on rows, or to specify
--   foreign keys.
newtype Selector t (n :: Nullability) (a :: Type) = Selector {selectorIndex :: Int}

-- * Creation

-- | A selector indicating the nth (zero-based) column of a table.
--
--   Will cause errors in queries during compilation, execution, or both,
--   unless handled with extreme care. You really shouldn't use it at all.
unsafeSelector :: SqlType a => Int -> Selector t n a
unsafeSelector = Selector

-- * Operation

-- | Extract the given column from the given row.
-- Row 全体の Nullability(rown) と Select対象 a の nullbilty(aIsNullabe) 両方を考慮する必要がある。
(!) ::
    SqlType a =>
    Row s n0 t ->
    Selector t n1 a ->
    Col s (n0 :.: n1) a
(Many xs) ! (Selector i) = case xs !! i of Untyped x -> One (unsafeCoerce x)

infixl 9 !

-- * IsLabel instance(by HasOrderedField)

instance
    ( HasOrderedField name t v
    , v ~ C n a
    , SqlType a
    ) =>
    IsLabel name (Selector t n a)
    where
    fromLabel = unsafeSelector $ fieldIndex @name @t

-- * HasOrderedField type class

class HasOrderedField (name :: Symbol) a v | name a -> v where
    fieldIndex :: Int

-- * CS instance

instance (Has 0 name cs ~ '(i, v), KnownNat i) => HasOrderedField name (CS cs) v where
    fieldIndex = fromIntegral $ natVal (Proxy :: Proxy i)

type family Has (index :: Nat) (name :: Symbol) (cs :: [Type]) where
    Has index name (name :- v ': _cs) = '(index, v)
    Has index name (_name :- _v ': cs') = Has (index + 1) name cs'

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
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SQL.Selector where

import Data.CoalesceMaybe (CoalesceMaybe)
import Data.Data (Proxy (Proxy))
import Database.Nelda.SQL.Col (Col (One))
import Database.Nelda.SQL.Row (Row (Many))
import Database.Nelda.SQL.Types (UntypedCol (Untyped))
import Database.Nelda.SqlType (SqlType)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (Symbol, natVal)
import qualified JRec.Internal as JRec
import Unsafe.Coerce (unsafeCoerce)

-- | A column selector. Column selectors can be used together with the '!' and
--   'with' functions to get and set values on rows, or to specify
--   foreign keys.
newtype Selector t a = Selector {selectorIndex :: Int}

-- * Creation

-- | A selector indicating the nth (zero-based) column of a table.
--
--   Will cause errors in queries during compilation, execution, or both,
--   unless handled with extreme care. You really shouldn't use it at all.
unsafeSelector :: SqlType a => Int -> Selector t a
unsafeSelector = Selector

-- * Operation

-- | Extract the given column from the given row.
-- TODO: coerce でいいかと思ったがコンパイルエラーになる。
(!) :: SqlType a => Row s t -> Selector t a -> Col s a
(Many xs) ! (Selector i) = case xs !! i of Untyped x -> One (unsafeCoerce x)

infixl 9 !

-- | Extract the given column from the given nullable row.
--   Nullable rows usually result from left joins.
--   If a nullable column is extracted from a nullable row, the resulting
--   nested @Maybe@s will be squashed into a single level of nesting.
(?) :: SqlType a => Row s (Maybe t) -> Selector t a -> Col s (CoalesceMaybe (Maybe a))
(Many xs) ? (Selector i) = case xs !! i of Untyped x -> One (unsafeCoerce x)

infixl 9 ?

-- * IsLabel instance(by HasOrderedField)

instance
    ( HasOrderedField name t type_
    , type_ ~ a
    , SqlType a
    ) =>
    IsLabel name (Selector t a)
    where
    fromLabel = unsafeSelector $ fieldIndex @name @t

-- * HasOrderedField type class

class HasOrderedField (name :: Symbol) a type_ | name a -> type_ where
    fieldIndex :: Int

-- * JRec instance

instance (JRec.Has name lts v, v ~ type_) => HasOrderedField name (JRec.Rec lts) type_ where
    fieldIndex = fromIntegral $ natVal (Proxy :: Proxy (JRec.RecTyIdxH 0 name lts))

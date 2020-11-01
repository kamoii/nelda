{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Database.Nelda.SQL.Row where

import Database.Nelda.SQL.Types (Nullability, UntypedCol)

-- | A database row. A row is a collection of one or more columns.
newtype Row s (n :: Nullability) a = Many [UntypedCol]

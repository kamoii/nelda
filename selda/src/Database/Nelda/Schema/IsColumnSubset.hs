{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Nelda.Schema.IsColumnSubset where

import Database.Nelda.Schema.Column (Column)

import GHC.TypeLits (TypeError, ErrorMessage(..), Symbol)
import GHC.Base (Constraint)

-- * Column Subset Checker

{-
Uniq key や (複数キーの)Primary だと カラム集合を指定するが,
それがテーブルのカラム集合に含まれているかのチェックをかけたい。
そのためチェックを行なう type  family。
エラーメッセージ がまともになるよう頑張る必要がある。
-}

-- tableColunmns = '[Column .., Column ...,]
-- columnNames = '[Symbol]
type family IsColumnSubset (tableName :: Symbol) (tableColumns :: [*]) (columnNames :: [Symbol]) :: Constraint where
    IsColumnSubset _ _ '[] = ()
    IsColumnSubset tname tcolumns (c ': cs) = (IsColumnInclude tname tcolumns c, IsColumnSubset tname tcolumns cs)

type family IsColumnInclude (tableName :: Symbol) (tableColumns :: [*]) (columnName :: Symbol) :: Constraint where
    IsColumnInclude _tname '[] _colName = TypeError ('Text "wtf")
    IsColumnInclude _tname (Column colName _ _ _ _ _ ': _tcs) colName = ()
    IsColumnInclude tname (_ ': tcs) column = IsColumnInclude tname tcs column

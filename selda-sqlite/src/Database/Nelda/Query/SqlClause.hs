{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Nelda.Query.SqlClause where

import Database.Nelda.Schema as Nelda (ColumnName(..))
import Database.Nelda.Schema (Table(..), Column(..), ColumnNull(..), Columns(..), AnyColumn(..))
import Database.Nelda.Query.Monad (renameAll, sources, Query(..))
import Database.Nelda.SQL.Row (Row(Many), Row)
import Database.Nelda.SQL.Types (mkColName, Exp(Col), UntypedCol(Untyped), sqlFrom, hideRenaming)
import Database.Nelda.SQL.Types (SqlSource(TableName))

import Control.Monad.State.Strict (get, put)
import JRec

-- * SELECT

type family ToQueryRecordField column :: * where
    ToQueryRecordField (Column name _ sqlType nullabilty _ _) =
        name := QueryTypeColumnNullWrapping nullabilty sqlType

type family QueryTypeColumnNullWrapping (nullabilty :: ColumnNull) (target :: *) :: * where
    QueryTypeColumnNullWrapping 'NotNull t = t
    QueryTypeColumnNullWrapping 'Nullable t = Maybe t
    QueryTypeColumnNullWrapping 'ImplicitNotNull t = t

type family ToQueryRecordFields columns :: [*] where
    ToQueryRecordFields '[] = '[]
    ToQueryRecordFields (column ': cs) = (ToQueryRecordField column ': ToQueryRecordFields cs)

select
    :: ( fields ~ ToQueryRecordFields cols )
    => Table name cols
    -> Query s (Row s (Rec fields))
select Table{tabName, tabColumns} = Query $ do
    -- 各カラムに一意的な名前の割りふり
    -- renameAll :: [UntypedExp] -> State GenState [SomeCol]
    rns <- renameAll $ columnsToUntypedCols tabColumns
    st <- get
    put $ st {sources = sqlFrom rns (TableName tabName) : sources st}
    return $ Many (map hideRenaming rns)
  where
    columnsToUntypedCols (Columns anyColumns) =
        map (\(AnyColumn column) -> columnToUntypedCol column) anyColumns

    columnToUntypedCol Column{colName=ColumnName name} =
        Untyped $ Col $ mkColName name

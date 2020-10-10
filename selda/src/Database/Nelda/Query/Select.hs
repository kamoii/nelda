{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Database.Nelda.Query.Select where

import Database.Nelda.Schema.Types as Nelda (ColumnName(..), TableName(..))
import Database.Nelda.Schema (Table(..), Column(..), Nullability(..), Columns(..), AnyColumn(..))

import Database.Selda.Core.Types (mkColName, mkTableName)
import Database.Selda.Query.Type (Query(..), sources, renameAll)
import Database.Selda.Table.Type (colExpr)
import Database.Selda.Column (Row(Many))
import Database.Selda.SQL (sqlFrom, hideRenaming, Exp(Col), UntypedCol(..))
import Database.Selda.SQL as Selda (SqlSource(TableName))

import Control.Monad.State.Strict (get, put)
import JRec

-- | Convert Table columns type to jrec's Rec fields.
type family ColumnToRecField column :: * where
    ColumnToRecField (Column name _ sqlType 'NotNull _) = name := sqlType
    ColumnToRecField (Column name _ sqlType 'Nullable _) = name := Maybe sqlType

type family ColumnsToRecFields columns :: [*] where
    ColumnsToRecFields '[] = '[]
    ColumnsToRecFields (column ': cs) = (ColumnToRecField column ': ColumnsToRecFields cs)

select
    :: ( fields ~ ColumnsToRecFields cols )
    => Table name cols
    -> Query s (Row s (Rec fields))
select Table{tabName, tabColumns} = Query $ do
    -- 各カラムに一意的な名前の割りふり
    -- renameAll :: [UntypedExp] -> State GenState [SomeCol]
    rns <- renameAll $ columnsToUntypedCols tabColumns
    st <- get
    put $ st {sources = sqlFrom rns (Selda.TableName $ convertTableName tabName) : sources st}
    return $ Many (map hideRenaming rns)
  where
    columnsToUntypedCols (Columns anyColumns) =
        map (\(AnyColumn column) -> columnToUntypedCol column) anyColumns

    columnToUntypedCol Column{colName=ColumnName name} =
        Untyped $ Col $ mkColName name

    convertTableName (Nelda.TableName name) =
        mkTableName name


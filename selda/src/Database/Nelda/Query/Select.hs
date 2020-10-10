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

{-

カラムを参照する式(e.g. foo.age) は ColumnInfo の colExpr に格納されている。
これは table 関数でレコード型からカラム情報を抽出するときに
テーブル内で一意的な名前になるようID(数値)を割り当てられる(Genericモジュール)。
さらに renameAll によりSQL全体を通じて一意的な名前になるようにする。

 , foo.age as XX_age_3

計算は Database.Selda.Genericモジュールの

:         , colExpr = Untyped (Col name')


data UntypedExp where
  Untyped :: !(Exp a) -> UntypedExp

-- | A type-erased column, which may also be renamed.
--   Only for internal use.
data SomeCol where
  Some  :: !(Exp a) -> SomeCol
  Named :: !ColName -> !(Exp a) -> SomeCol

-- | Generate a unique name for the given column.
rename :: UntypedExp -> State GenState [SomeCol]
rename (Untyped col) = do
    n <- freshId
    return [Named (newName n) col]
  where
    newName ns =
      case col of
        Col n -> addColSuffix n $ "_" <> pack (show ns)
        _     -> mkColName $ "tmp_" <> pack (show ns)

-- | Turn a renamed column back into a regular one.
--   If the column was renamed, it will be represented by a literal column,
--   and not its original expression.
hideRenaming :: SomeCol -> UntypedExp
hideRenaming (Named n _) = Untyped (Col n)
hideRenaming (Some c)    = Untyped c
-}

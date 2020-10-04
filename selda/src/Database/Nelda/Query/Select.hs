{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Database.Nelda.Query.Select where

import Database.Nelda.Schema (Table(..), Column, Nullability(..))

import Database.Selda.Query.Type (Query(..), sources, renameAll)
import Database.Selda.Table.Type (colExpr)
import Database.Selda.Column (Row(Many))
import Database.Selda.SQL (sqlFrom, SqlSource(TableName), hideRenaming)

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
select Table{tabName=_, tabColumns=_} = Query $ do
    -- 各カラムに一意的な名前の割りふり
    -- renameAll :: [UntypedCol] -> State GenState [SomeCol]
    rns <- renameAll $ map colExpr cs
    st <- get
    put $ st {sources = sqlFrom rns (TableName name) : sources st}
    return $ Many (map hideRenaming rns)
  where
    cs = undefined
    name = undefined

{-

カラムを参照する式(e.g. foo.age) は ColumnInfo の colExpr に格納されている。
これは table 関数でレコード型からカラム情報を抽出するときに
テーブル内で一意的な名前になるようID(数値)を割り当てられる(Genericモジュール)。
さらに renameAll によりSQL全体を通じて一意的な名前になるようにする。

 , foo.age as XX_age_3

計算は Database.Selda.Genericモジュールの

:         , colExpr = Untyped (Col name')


data UntypedCol where
  Untyped :: !(Exp a) -> UntypedCol

-- | A type-erased column, which may also be renamed.
--   Only for internal use.
data SomeCol where
  Some  :: !(Exp a) -> SomeCol
  Named :: !ColName -> !(Exp a) -> SomeCol

-- | Generate a unique name for the given column.
rename :: UntypedCol -> State GenState [SomeCol]
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
hideRenaming :: SomeCol -> UntypedCol
hideRenaming (Named n _) = Untyped (Col n)
hideRenaming (Some c)    = Untyped c
-}

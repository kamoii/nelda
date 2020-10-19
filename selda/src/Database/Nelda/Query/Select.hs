{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Nelda.Query.Select where

import Database.Nelda.Schema as Nelda (ColumnName(..), TableName(..))
import Database.Nelda.Schema (Table(..), Column(..), ColumnNull(..), Columns(..), AnyColumn(..))
import Database.Nelda.SqlType (SqlType(..), SqlValue)

import Database.Selda.Core.Types (mkColName, mkTableName)
import Database.Selda.Query.Type (Query(..), sources, renameAll)
import Database.Selda.Table.Type (colExpr)
import Database.Selda.Column (Row(Many))
import Database.Selda.SQL (sqlFrom, hideRenaming, Exp(Col), UntypedCol(..))
import Database.Selda.SQL as Selda (SqlSource(TableName))
import Database.Selda.SqlRow (SqlRow(..), ResultReader(..))


import Control.Monad.State.Strict (get, put)
import qualified Data.List as List
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, natVal, KnownSymbol)
import JRec
import JRec.Internal as JRec (RecSize, create, unsafeRNil, unsafeRCons)
import Control.Monad.ST (ST)
import Control.Monad.State.Strict (state)

-- | Convert Table columns type to jrec's Rec fields.
type family ToQueryRecordField column :: * where
    ToQueryRecordField (Column name _ sqlType 'NotNull _) = name := sqlType
    ToQueryRecordField (Column name _ sqlType 'Nullable _) = name := Maybe sqlType

type family ToQueryRecordFields columns :: [*] where
    ToQueryRecordFields '[] = '[]
    ToQueryRecordFields (column ': cs) = (ToQueryRecordField column ': ToQueryRecordFields cs)

-- Query の結果から値を抽出するための型クラス。
-- NOTE: SqlRow という名前が微妙という説
instance (Typeable fields, UnsafeSqlRowRecord (Rec fields)) => SqlRow (Rec fields) where
    -- ResultReader a の実態は State [SqlValue] a。
    -- [SqlValue]状態から必要な値を先頭から取りだし a を作成する State アクションを実装すればいい。
    nextResult :: ResultReader (Rec fields)
    nextResult = R $ do
        vals <- state $ List.splitAt (nestedCols (Proxy :: Proxy (Rec fields)))
        pure $ JRec.create $ _recordBuild 0 vals

    -- a を抽出するのに必要なカラムの数。
    -- Database.Selda.Generic で定義されているものに関しては再帰的な GSqlRow を許容している？？
    -- うーん,少なくとも Rec の場合は 1 フィールド 1 value でいいような。
    -- なので単純にフィールド数を返す
    nestedCols :: Proxy (Rec fields) -> Int
    nestedCols = _recordSize

-- internal
class UnsafeSqlRowRecord record where
    _recordBuild :: Int -> [SqlValue] -> ST s record
    _recordSize :: Proxy record -> Int

instance UnsafeSqlRowRecord (Rec '[]) where
    _recordBuild size [] = JRec.unsafeRNil size
    _recordBuild _ _ = error "Implementation Error"
    _recordSize _ = 0

instance
    ( UnsafeSqlRowRecord (Rec lts)
    , SqlType t
    , KnownNat (RecSize lts)
    , KnownSymbol l
    ) => UnsafeSqlRowRecord (Rec (l := t ': lts)) where
    _recordBuild size (v:vs) = do
        rec' <- _recordBuild (size+1) vs
        JRec.unsafeRCons (undefined := fromSqlValue v) rec'
    _recordBuild _ _ = error "Implementation Error"
    _recordSize _ = _recordSize (Proxy :: Proxy (Rec lts)) + 1

select
    :: ( fields ~ ToQueryRecordFields cols )
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

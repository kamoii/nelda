{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.Query.ResultRow where

import Control.Monad.ST (ST)
import Control.Monad.State (state)
import qualified Data.List as List
import Data.Proxy (Proxy (Proxy))
import Database.Nelda.Backend.Types (SqlValue)
import Database.Nelda.Query.ResultReader
import Database.Nelda.SqlTypeClass (NullableSqlType (fromSqlValue'))
import GHC.TypeLits (KnownNat, KnownSymbol)
import JRec
import qualified JRec.Internal as JRec

-- Row s a から結果 a を得るための型クラス

-- TODO: rename to SqlRow and move Module.

class ResultRow a where
    -- | Read the next, potentially composite, result from a stream of columns.
    nextResult :: ResultReader a

    -- | The number of nested columns contained in this type.
    nestedCols :: Proxy a -> Int

-- * Rec Instance

-- Query の結果から値を抽出するための型クラス。
-- TODO: RecApply type class が使えるのでは???
instance (UnsafeResultRowRecord (Rec fields)) => ResultRow (Rec fields) where
    -- ResultReader a の実態は State [SqlValue] a。
    -- [SqlValue]状態から必要な値を先頭から取りだし a を作成する State アクションを実装すればいい。
    nextResult :: ResultReader (Rec fields)
    nextResult = ResultReader $ do
        vals <- state $ List.splitAt (nestedCols (Proxy :: Proxy (Rec fields)))
        pure $ JRec.create $ _recordBuild 0 vals

    -- a を抽出するのに必要なカラムの数。
    -- Database.Selda.Generic で定義されているものに関しては再帰的な GSqlRow を許容している？？
    -- うーん,少なくとも Rec の場合は 1 フィールド 1 value でいいような。
    -- なので単純にフィールド数を返す
    nestedCols :: Proxy (Rec fields) -> Int
    nestedCols = _recordSize

-- internal
class UnsafeResultRowRecord record where
    _recordBuild :: Int -> [SqlValue] -> ST s record
    _recordSize :: Proxy record -> Int

instance UnsafeResultRowRecord (Rec '[]) where
    _recordBuild size [] = JRec.unsafeRNil size
    _recordBuild _ _ = error "Implementation Error"
    _recordSize _ = 0

instance
    ( UnsafeResultRowRecord (Rec lts)
    , NullableSqlType t _n
    , KnownNat (JRec.RecSize lts)
    , KnownSymbol l
    ) =>
    UnsafeResultRowRecord (Rec (l := t ': lts))
    where
    _recordBuild size (v : vs) = do
        rec' <- _recordBuild (size + 1) vs
        JRec.unsafeRCons (undefined := fromSqlValue' v) rec'
    _recordBuild _ _ = error "Implementation Error"
    _recordSize _ = _recordSize (Proxy :: Proxy (Rec lts)) + 1

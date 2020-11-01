{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.Query.SqlRow where

import Control.Monad.ST (ST)
import Control.Monad.State (state)
import Data.Kind (Type)
import qualified Data.List as List
import Data.Proxy (Proxy (Proxy))
import Database.Nelda.Backend.Types (SqlValue)
import Database.Nelda.Query.ResultReader
import Database.Nelda.SqlTypeClass (SqlType)
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol)
import JRec
import qualified JRec.Internal as JRec

-- カラム集合

class SqlRow (a :: p) where
    type SqlRowRes a :: Type

    -- | Read the next, potentially composite, result from a stream of columns.
    fromSqlValues :: ResultReader (SqlRowRes a)

    -- | The number of nested columns contained in this type.
    -- fromSqlValues が消費する SqlValue の数
    consumeLength :: Proxy a -> Int

-- * Rec Instance

-- Query の結果から値を抽出するための型クラス。
-- TODO: RecApply type class が使えるのでは???
instance (UnsafeSqlRowJRec cs) => SqlRow (cs :: [Type]) where
    -- ResultReader a の実態は State [SqlValue] a。
    -- [SqlValue]状態から必要な値を先頭から取りだし a を作成する State アクションを実装すればいい。
    fromSqlValues :: ResultReader (Rec (JRecFields cs))
    fromSqlValues = ResultReader $ do
        vals <- state $ List.splitAt (consumeLength (Proxy :: Proxy (Rec fields)))
        pure $ JRec.create $ _recordBuild 0 vals

    -- a を抽出するのに必要なカラムの数。
    -- Database.Selda.Generic で定義されているものに関しては再帰的な GSqlRow を許容している？？
    -- うーん,少なくとも Rec の場合は 1 フィールド 1 value でいいような。
    -- なので単純にフィールド数を返す
    consumeLength :: Proxy (Rec fields) -> Int
    consumeLength = _recordSize

-- internal
class UnsafeSqlRowJRec cs where
    type JRecFields cs :: '[Type]
    _recordBuild :: Int -> [SqlValue] -> ST s (JRecFields cs)
    _recordSize :: Proxy cs -> Int

instance UnsafeSqlRowJRec '[] where
    type JRecFields '[] = '[]
    _recordBuild size [] = JRec.unsafeRNil size
    _recordBuild _ _ = error "Implementation Error"
    _recordSize _ = 0

instance
    ( UnsafeSqlRowJRec lts
    , FromSqlType n t wt
    , KnownSymbol l
    ) =>
    UnsafeSqlRowJRec (l := ((n :: Nullability), t) ': lts)
    where
    type JRecFields (l := (n, t) ': lts) = (l := wt ': JRecFields lts)
    _recordBuild size (v : vs) = do
        rec' <- _recordBuild (size + 1) vs
        JRec.unsafeRCons (JRec.FldProxy @l := fromSqlValue' v) rec'
    _recordBuild _ _ = error "Implementation Error"
    _recordSize _ = _recordSize (Proxy :: Proxy (Rec lts)) + 1

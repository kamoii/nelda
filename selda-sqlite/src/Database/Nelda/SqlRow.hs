{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SqlRow where

import Control.Monad.ST (ST)
import Control.Monad.State (state)
import Data.Kind (Type)
import qualified Data.List as List
import Data.Proxy (Proxy (Proxy))
import Database.Nelda.Backend.Types (SqlValue)
import Database.Nelda.Query.ResultReader
import Database.Nelda.SQL.Row (C, CS)
import Database.Nelda.SqlTypeConversion (FromSqlType, fromSqlValue')
import GHC.TypeLits (KnownNat, KnownSymbol)
import JRec
import qualified JRec.Internal as JRec

-- カラム集合
-- CC ["foo" := C 'NonNUll Int, "bar" := C 'Nullable String]  <->  Rec ["foo" := Int, "bar" := Maybe String]

class SqlRow row rec_ | row -> rec_, rec_ -> row where
    -- type SqlRowRes row = (rec_ :: Type) | rec_ -> row

    -- | Read the next, potentially composite, result from a stream of columns.
    fromSqlValues :: ResultReader rec_

    -- | The number of nested columns contained in this type.
    -- fromSqlValues が消費する SqlValue の数
    consumeLength :: Proxy row -> Int

-- * Rec Instance

-- Query の結果から値を抽出するための型クラス。
-- TODO: RecApply type class が使えるのでは???
instance (UnsafeSqlRowJRec cs rs) => SqlRow (CS cs) (JRec.Rec rs) where
    -- type SqlRowRes (CS cs) = JRec.Rec (JRecFields cs)

    -- ResultReader a の実態は State [SqlValue] a。
    -- [SqlValue]状態から必要な値を先頭から取りだし a を作成する State アクションを実装すればいい。
    fromSqlValues :: ResultReader (Rec rs)
    fromSqlValues = ResultReader $ do
        vals <- state $ List.splitAt (consumeLength (Proxy @(CS cs)))
        pure $ JRec.create $ _recordBuild @cs 0 vals

    -- a を抽出するのに必要なカラムの数。
    -- Database.Selda.Generic で定義されているものに関しては再帰的な GSqlRow を許容している？？
    -- うーん,少なくとも Rec の場合は 1 フィールド 1 value でいいような。
    -- なので単純にフィールド数を返す
    -- consumeLength :: Proxy (CS cs) -> Int
    consumeLength _ = _recordSize @cs @rs

-- ** Internal

class UnsafeSqlRowJRec (cs :: [Type]) (rs :: [Type]) | cs -> rs, rs -> cs where
    _recordBuild :: Int -> [SqlValue] -> ST s (JRec.Rec rs)
    _recordSize :: Int

instance UnsafeSqlRowJRec '[] '[] where
    _recordBuild size [] = JRec.unsafeRNil size
    _recordBuild _ _ = error "Implementation Error"
    _recordSize = 0

instance
    ( UnsafeSqlRowJRec cs' rs'
    , FromSqlType n t t'
    , KnownSymbol l
    , KnownNat (JRec.RecSize rs')
    , l ~ l'
    ) =>
    UnsafeSqlRowJRec (l := C n t ': cs') (l' := t' ': rs')
    where
    _recordBuild size (v : vs) = do
        rec' <- _recordBuild @cs' @rs' (size + 1) vs
        JRec.unsafeRCons (JRec.FldProxy @l := fromSqlValue' @n @t v) rec'
    _recordBuild _ _ = error "Implementation Error"
    _recordSize = _recordSize @cs' @rs' + 1

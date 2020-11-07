{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
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
import Database.Nelda.SQL.Row ((:-), C, CS)
import Database.Nelda.SqlTypeConversion (FromSqlType, fromSqlValue')
import GHC.TypeLits (KnownNat, KnownSymbol)
import qualified JRec.Internal as JRec
import Unsafe.Coerce (unsafeCoerce)

-- カラム集合
-- CC ["foo" := C 'NonNUll Int, "bar" := C 'Nullable String]  <->  Rec ["foo" := Int, "bar" := Maybe String]

class SqlRow row rec_ | row -> rec_, rec_ -> row where
    -- type SqlRowRes row = (rec_ :: Type) | rec_ -> row

    -- | Use when brigning rec into EDSL(row).
    reflectRec :: (forall n t t'. FromSqlType n t t' => t' -> r) -> rec_ -> [r]

    reflectRecGhost :: (forall n t t'. FromSqlType n t t' => Proxy t' -> r) -> Proxy rec_ -> [r]

    -- | Read the next, potentially composite, result from a stream of columns.
    fromSqlValues :: ResultReader rec_

    -- | The number of nested columns contained in this type.
    -- fromSqlValues が消費する SqlValue の数
    consumeLength :: Proxy row -> Int

-- * Rec Instance

-- Query の結果から値を抽出するための型クラス。
-- TODO: RecApply type class が使えるのでは???
instance (UnsafeSqlRowJRec cs rs) => SqlRow (CS cs) (JRec.Rec rs) where
    reflectRec f rec_ =
        List.reverse $ _reflectRec @cs 0 rec_ (\t' xs -> f t' : xs) []

    reflectRecGhost f _ =
        List.reverse $ _reflectRecGhost @cs 0 (\p xs -> f p : xs) []

    -- ResultReader a の実態は State [SqlValue] a。
    -- [SqlValue]状態から必要な値を先頭から取りだし a を作成する State アクションを実装すればいい。
    fromSqlValues :: ResultReader (JRec.Rec rs)
    fromSqlValues = ResultReader $ do
        vals <- state $ List.splitAt (consumeLength (Proxy @(CS cs)))
        pure $ JRec.create $ _buildRec @cs 0 vals

    -- a を抽出するのに必要なカラムの数。
    -- Database.Selda.Generic で定義されているものに関しては再帰的な GSqlRow を許容している？？
    -- うーん,少なくとも Rec の場合は 1 フィールド 1 value でいいような。
    -- なので単純にフィールド数を返す
    -- consumeLength :: Proxy (CS cs) -> Int
    consumeLength _ = _sizeRec @cs @rs

-- ** Internal

-- TODO: ラベルの重複チェック

-- 適切な TypeApplication を与えないと
class UnsafeSqlRowJRec (cs :: [Type]) (rs :: [Type]) | cs -> rs, rs -> cs where
    -- これは JRec の RecApply の機能と似ているが,この型クラスが持つと思われるためここで。
    -- Int の初期値は0 を, Rec rs' には Rec rs を渡す必要がある。
    _reflectRec ::
        forall _rs r.
        Int ->
        JRec.Rec _rs ->
        (forall n t t'. FromSqlType n t t' => t' -> r -> r) ->
        r ->
        r

    _reflectRecGhost ::
        Int ->
        (forall n t t'. FromSqlType n t t' => Proxy t' -> r -> r) ->
        r ->
        r

    _buildRec :: Int -> [SqlValue] -> ST s (JRec.Rec rs)

    _sizeRec :: Int

instance UnsafeSqlRowJRec '[] '[] where
    _reflectRec _ _ _ r = r
    _reflectRecGhost _ _ r = r
    _buildRec size [] = JRec.unsafeRNil size
    _buildRec _ _ = error "Implementation Error"
    _sizeRec = 0

instance
    ( UnsafeSqlRowJRec cs' rs'
    , FromSqlType n t t'
    , KnownSymbol l
    , KnownNat (JRec.RecSize rs')
    , l ~ l'
    ) =>
    UnsafeSqlRowJRec (l :- C n t ': cs') (l' JRec.:= t' ': rs')
    where
    _reflectRec index rec_ f r =
        let t' = unsafeCoerce (JRec.unsafeGet index rec_) :: t'
            r' = f t' r
         in _reflectRec @cs' (index + 1) rec_ f r'

    _reflectRecGhost index f r =
        let p = Proxy @t'
            r' = f p r
         in _reflectRecGhost @cs' (index + 1) f r'

    _buildRec size (v : vs) = do
        rec' <- _buildRec @cs' (size + 1) vs
        JRec.unsafeRCons (JRec.FldProxy @l JRec.:= fromSqlValue' @n @t v) rec'
    _buildRec _ _ = error "Implementation Error"

    _sizeRec = _sizeRec @cs' + 1

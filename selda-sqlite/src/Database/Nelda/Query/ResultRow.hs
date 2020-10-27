{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.Query.ResultRow where

import Database.Nelda.Query.ResultReader
import Data.Typeable (Typeable)
import GHC.Generics
import Data.Proxy (Proxy(Proxy))
import Database.Nelda.SqlType (fromSqlValue, SqlType)
import qualified GHC.TypeLits as TL
import qualified Database.Nelda.Backend.Types as BE
import Control.Monad (liftM2)
import Control.Monad.State (state, MonadState(get))
import qualified Data.List as List
import qualified JRec.Internal as JRec
import JRec
import Database.Nelda.Backend.Types (SqlValue)
import GHC.TypeLits (KnownSymbol, KnownNat)
import Control.Monad.ST (ST)

-- Row s a から結果 a を得るための型クラス

class Typeable a => ResultRow a where
    -- | Read the next, potentially composite, result from a stream of columns.
    nextResult :: ResultReader a
    default nextResult :: (Generic a, GSqlRow (Rep a)) => ResultReader a
    nextResult = to <$> gNextResult

    -- | The number of nested columns contained in this type.
    nestedCols :: Proxy a -> Int
    default nestedCols :: (Generic a, GSqlRow (Rep a)) => Proxy a -> Int
    nestedCols _ = gNestedCols (Proxy :: Proxy (Rep a))

-- * Generic derivation for ResultRow

class GSqlRow f where
    gNextResult :: ResultReader (f x)
    gNestedCols :: Proxy f -> Int

instance SqlType a => GSqlRow (K1 i a) where
    gNextResult = K1 <$> fromSqlValue <$> next
    gNestedCols _ = 1

instance GSqlRow f => GSqlRow (M1 c i f) where
    gNextResult = M1 <$> gNextResult
    gNestedCols _ = gNestedCols (Proxy :: Proxy f)

instance (GSqlRow a, GSqlRow b) => GSqlRow (a :*: b) where
    gNextResult = liftM2 (:*:) gNextResult gNextResult
    gNestedCols _ = gNestedCols (Proxy :: Proxy a) + gNestedCols (Proxy :: Proxy b)

instance
  (TL.TypeError
    ( 'TL.Text "Selda currently does not support creating tables from sum types."
      'TL.:$$:
      'TL.Text "Restrict your table type to a single data constructor."
    )) => GSqlRow (a :+: b) where
    gNextResult = error "unreachable"
    gNestedCols = error "unreachable"

-- * Maybe Instance
instance ResultRow a => ResultRow (Maybe a) where
  nextResult = do
      xs <- ResultReader get
      if all BE.isSqlValueNull (take (nestedCols (Proxy :: Proxy a)) xs)
          then return Nothing
          else Just <$> nextResult

  nestedCols _ = nestedCols (Proxy :: Proxy a)

-- * Tupple Instance

instance
    ( Typeable (a, b)
    , GSqlRow (Rep (a, b))
    ) => ResultRow (a, b)
instance
    ( Typeable (a, b, c)
    , GSqlRow (Rep (a, b, c))
    ) => ResultRow (a, b, c)
instance
    ( Typeable (a, b, c, d)
    , GSqlRow (Rep (a, b, c, d))
    ) => ResultRow (a, b, c, d)
instance
    ( Typeable (a, b, c, d, e)
    , GSqlRow (Rep (a, b, c, d, e))
    ) => ResultRow (a, b, c, d, e)
instance
    ( Typeable (a, b, c, d, e, f)
    , GSqlRow (Rep (a, b, c, d, e, f))
    ) => ResultRow (a, b, c, d, e, f)
instance
    ( Typeable (a, b, c, d, e, f, g)
    , GSqlRow (Rep (a, b, c, d, e, f, g))
    ) => ResultRow (a, b, c, d, e, f, g)

-- * Rec Instance

-- Query の結果から値を抽出するための型クラス。
-- TODO: RecApply type class が使えるのでは???
instance (Typeable fields, UnsafeResultRowRecord (Rec fields)) => ResultRow (Rec fields) where
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
    , SqlType t
    , KnownNat (JRec.RecSize lts)
    , KnownSymbol l
    ) => UnsafeResultRowRecord (Rec (l := t ': lts)) where
    _recordBuild size (v:vs) = do
        rec' <- _recordBuild (size+1) vs
        JRec.unsafeRCons (undefined := fromSqlValue v) rec'
    _recordBuild _ _ = error "Implementation Error"
    _recordSize _ = _recordSize (Proxy :: Proxy (Rec lts)) + 1

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SqlTypeConversion where

import Data.Kind (Type)
import Database.Nelda.Backend.Types (SqlValue)
import qualified Database.Nelda.Backend.Types as BE
import Database.Nelda.SQL.Nullability (Nullability (..))
import Database.Nelda.SqlType (SqlType)
import Database.Nelda.SqlTypeClass (SqlType (fromSqlValue))

-- | SqlType x Nullability <->  a OR Maybe a
-- Nelda DSL と Haskell の境界で変換するための型クラス
-- Nelda DSL 内で見ることは基本ないはず。
-- values で Haskellの値(NULLは Maybeで表現)を Nelda DSL に持ち込む際や,
-- query の結果を Haskell の値として受け取る際に利用する。
-- DNS 内では Nullability によって明示的に管理する。

-- * ToSqlType

-- Maybe + その他(OVERLAPPABLE)パターンを使っているため, assiciated type family は使えない
-- (intances は overlap しても assiociated type family は overlap できないため)。
-- Functional Dependecy を使うと kind が * -> * -> Constraint になってしまい,
-- kind * -> Constraint を受け取る JRec の type family が使えなくなる。
-- そのため closed type family を使っている。

type family ToSqlTypeNullability wt :: Nullability where
    ToSqlTypeNullability (Maybe _) = 'Nullable
    ToSqlTypeNullability _ = 'NonNull

type family ToSqlTypeType wt :: Type where
    ToSqlTypeType (Maybe t) = t
    ToSqlTypeType t = t

class (SqlType (ToSqlTypeType wt)) => ToSqlType wt

instance SqlType a => ToSqlType (Maybe a)

instance
    {-# OVERLAPPABLE #-}
    ( SqlType a
    , ToSqlTypeType a ~ a
    ) =>
    ToSqlType a

-- * FromSqlType

class
    SqlType t =>
    FromSqlType
        (n :: Nullability)
        (t :: Type)
        (wt :: Type)
        | n t -> wt
    where
    fromSqlValue' :: SqlValue -> wt

instance SqlType t => FromSqlType 'NonNull t t where
    fromSqlValue' = fromSqlValue

instance SqlType t => FromSqlType 'Nullable t (Maybe t) where
    fromSqlValue' v
        | BE.isSqlValueNull v = Nothing
        | otherwise = Just $ fromSqlValue v

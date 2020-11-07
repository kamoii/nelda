{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SqlTypeConversion where

import Data.Kind (Type)
import Database.Nelda.Backend.Types (SqlValue, nullSqlParam)
import qualified Database.Nelda.Backend.Types as BE
import Database.Nelda.SQL.Nullability (Nullability (..))
import Database.Nelda.SQL.Types (Lit, mkLit, mkNullLit)
import Database.Nelda.SqlType (SqlParam, SqlType)
import Database.Nelda.SqlTypeClass (SqlType (fromSqlValue, toSqlParam))

-- | SqlType x Nullability <->  a OR Maybe a
--
-- なので単純にToSqlType/FroSqlType は若干嘘で ToNullableSqlType/FromNullableSqlType
-- という名前のほうが良いかな..
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

-- * FromSqlType

-- 逆の Bar は成功するという罠...
-- type family Foo (b :: Type) = (tt :: Type) | tt -> b where
--     Foo (B 'Nullable t) = Maybe t
--     Foo (B 'NonNull t) = t

-- おお, inject type family 成功か？
-- data B (n :: Nullability) (t :: Type)

data DMaybe (n :: Nullability) (t :: Type)

type family DecomposeMaybe (t :: Type) = (b :: Type) | b -> t where
    DecomposeMaybe (Maybe t) = DMaybe 'Nullable t
    DecomposeMaybe t = DMaybe 'NonNull t

class (DecomposeMaybe t' ~ DMaybe n t) => NullOrMaybe (n :: Nullability) (t :: Type) (t' :: Type) | n t -> t', t' -> n t
instance (DecomposeMaybe t' ~ DMaybe n t) => NullOrMaybe n t t'

class (SqlType t, NullOrMaybe n t t') => FromSqlType (n :: Nullability) (t :: Type) (t' :: Type) where
    fromSqlValue' :: SqlValue -> t'
    toSqlParam' :: t' -> SqlParam
    mkLit' :: t' -> Lit t

instance (SqlType t, NullOrMaybe 'NonNull t t) => FromSqlType 'NonNull t t where
    fromSqlValue' = fromSqlValue
    toSqlParam' = toSqlParam
    mkLit' = mkLit

instance SqlType t => FromSqlType 'Nullable t (Maybe t) where
    fromSqlValue' v
        | BE.isSqlValueNull v = Nothing
        | otherwise = Just $ fromSqlValue v

    toSqlParam' Nothing = nullSqlParam
    toSqlParam' (Just a) = toSqlParam a

    mkLit' Nothing = mkNullLit
    mkLit' (Just a) = mkLit a

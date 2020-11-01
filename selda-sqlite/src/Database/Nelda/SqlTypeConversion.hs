{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SqlTypeConversion where

import Data.Kind (Type)
import Database.Nelda.Backend.Types (SqlValue)
import qualified Database.Nelda.Backend.Types as BE
import Database.Nelda.SQL.Nullability (Nullability (..))
import Database.Nelda.SqlType (SqlType)
import Database.Nelda.SqlTypeClass (SqlType (fromSqlValue))
import Database.Nelda.SQL.Types (mkLit, mkNullLit, Lit)

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

type family ToSqlTypeNullability wt :: Nullability where
    ToSqlTypeNullability (Maybe _) = 'Nullable
    ToSqlTypeNullability _ = 'NonNull

type family ToSqlTypeType wt :: Type where
    ToSqlTypeType (Maybe t) = t
    ToSqlTypeType t = t

class (SqlType (ToSqlTypeType wt)) => ToSqlType wt where
    mkLit' :: wt -> Lit (ToSqlTypeType wt)

instance SqlType a => ToSqlType (Maybe a) where
    mkLit' Nothing = mkNullLit
    mkLit' (Just a) = mkLit a

instance
    {-# OVERLAPPABLE #-}
    ( SqlType a
    , ToSqlTypeType a ~ a
    ) =>
    ToSqlType a where
    mkLit' = mkLit

-- * FromSqlType

class SqlType t => FromSqlType (n :: Nullability) (t :: Type) where
    type FromSqlTypeTargetType n t :: Type
    fromSqlValue' :: SqlValue -> FromSqlTypeTargetType n t

instance SqlType t => FromSqlType 'NonNull t where
    type FromSqlTypeTargetType 'NonNull t = t
    fromSqlValue' = fromSqlValue

instance SqlType t => FromSqlType 'Nullable t where
    type FromSqlTypeTargetType 'Nullable t = Maybe t
    fromSqlValue' v
        | BE.isSqlValueNull v = Nothing
        | otherwise = Just $ fromSqlValue v

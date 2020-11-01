{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SqlRowConversion where

import Control.Monad.State.Strict (MonadState (get))
import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import qualified Database.Nelda.Backend.Types as BE
import Database.Nelda.Query.ResultReader (ResultReader (ResultReader))
import Database.Nelda.SQL.Nullability (Nullability (..))
import Database.Nelda.SqlRow (SqlRow, SqlRowRes, consumeLength, fromSqlValues)

-- | SqlRow x Nullability <-> Rec
--
-- SqlTypeConversion と対を為す存在

-- * ToSqlRow

-- -- Maybe + その他(OVERLAPPABLE)パターンを使っているため, assiciated type family は使えない
-- -- (intances は overlap しても assiociated type family は overlap できないため)。
-- -- Functional Dependecy を使うと kind が * -> * -> Constraint になってしまい,
-- -- kind * -> Constraint を受け取る JRec の type family が使えなくなる。
-- -- そのため closed type family を使っている。
--
-- type family ToSqlTypeNullability wt :: Nullability where
--     ToSqlTypeNullability (Maybe _) = 'Nullable
--     ToSqlTypeNullability _ = 'NonNull
--
-- type family ToSqlTypeType wt :: Type where
--     ToSqlTypeType (Maybe t) = t
--     ToSqlTypeType t = t
--
-- class (SqlType (ToSqlTypeType wt)) => ToSqlType wt
--
-- instance SqlType a => ToSqlType (Maybe a)
--
-- instance
--     {-# OVERLAPPABLE #-}
--     ( SqlType a
--     , ToSqlTypeType a ~ a
--     ) =>
--     ToSqlType a

-- * FromSqlType

class SqlRow t => FromSqlRow (n :: Nullability) t where
    type FromSqlRowTargetType n t :: Type
    fromSqlValues' :: ResultReader (FromSqlRowTargetType n t)

instance SqlRow t => FromSqlRow 'NonNull t where
    type FromSqlRowTargetType 'NonNull t = SqlRowRes t
    fromSqlValues' = fromSqlValues @t

instance SqlRow t => FromSqlRow 'Nullable t where
    type FromSqlRowTargetType 'Nullable t = Maybe (SqlRowRes t)
    fromSqlValues' = do
        xs <- ResultReader get
        if all BE.isSqlValueNull (take (consumeLength (Proxy @t)) xs)
            then pure Nothing
            else Just <$> fromSqlValues @t

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

module Database.Nelda.SqlRowConversion where

import Control.Monad.State.Strict (put, MonadState (get))
import Data.Data (Proxy (Proxy))
import qualified Database.Nelda.Backend.Types as BE
import Database.Nelda.Query.ResultReader (ResultReader (ResultReader))
import Database.Nelda.SQL.Nullability (Nullability (..))
import Database.Nelda.SqlRow (SqlRow, consumeLength, fromSqlValues)
import qualified Data.List as List

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

-- * SqlTypeConv

class FromSqlRow (n :: Nullability) row val | n row -> val where
    fromSqlValues' :: ResultReader val

instance (SqlRow row rec_) => FromSqlRow 'NonNull row rec_ where
    fromSqlValues' = fromSqlValues @row

instance (SqlRow row rec_) => FromSqlRow 'Nullable row (Maybe rec_) where
    fromSqlValues' = do
        xs <- ResultReader get
        let (h, t) = List.splitAt (consumeLength (Proxy @row)) xs
        if all BE.isSqlValueNull h
            then Nothing <$ ResultReader (put t)
            else Just <$> fromSqlValues @row

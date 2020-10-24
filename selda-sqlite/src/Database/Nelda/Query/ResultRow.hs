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
import Control.Monad.State (MonadState(get))

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

-- * Various instances
instance ResultRow a => ResultRow (Maybe a) where
  nextResult = do
      xs <- ResultReader get
      if all BE.isSqlValueNull (take (nestedCols (Proxy :: Proxy a)) xs)
          then return Nothing
          else Just <$> nextResult

  nestedCols _ = nestedCols (Proxy :: Proxy a)

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

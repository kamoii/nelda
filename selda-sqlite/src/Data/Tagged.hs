{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tagged where

import GHC.TypeLits (symbolVal, KnownSymbol)
import GHC.Base (Symbol)
import Data.Proxy (Proxy(..))
import GHC.OverloadedLabels (IsLabel(..))
import Data.String (fromString, IsString)

-- * Tagged
-- OverloadedLabels を活用するために

newtype Tagged a (s :: Symbol) = Tagged { untag :: a }
    deriving (Eq, Ord, Show, IsString)

instance (KnownSymbol s, s ~ s', IsString a) => IsLabel s (Tagged a s') where
    fromLabel = fromString $ symbolVal (Proxy :: Proxy s)

type family TaggedLabel t :: Symbol where
    TaggedLabel (Tagged _ l) = l

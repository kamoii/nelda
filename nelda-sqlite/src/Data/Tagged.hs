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
import Data.Proxy (Proxy(..))
import GHC.OverloadedLabels (IsLabel(..))
import Data.String (fromString, IsString)

-- * Tagged
--
-- OverloadedLabels を活用するために tagged パッケージは使わず独自に定義。
-- 細かい話だが, tag paraeter が後ろのほうが見やすい。
-- あと s は Symbol に限定しない

newtype Tagged a (s :: k) = Tagged { untag :: a }
    deriving (Eq, Ord, Show, IsString)

instance (KnownSymbol s, s ~ s', IsString a) => IsLabel s (Tagged a s') where
    fromLabel = fromString $ symbolVal (Proxy :: Proxy s)

type family TaggedLabel t :: k where
    TaggedLabel (Tagged _ l) = l

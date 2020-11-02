{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module JRecExended where

import Data.Constraint (Dict (Dict))
import Data.Data (Proxy (Proxy))
import Data.Kind (Constraint)
import GHC.TypeLits (KnownSymbol, symbolVal)
import JRec
import JRec.Internal

-- | JRec に足りない機能を実装

-- | Machinery needed to implement 'reflectRec'
class RecApply3 (rts :: [*]) (lts :: [*]) (c :: * -> * -> * -> Constraint) where
    recApply3 ::
        (forall a n t. Dict (c n t a) -> String -> a -> b -> b) ->
        Rec rts ->
        Proxy lts ->
        b ->
        b
    recApply3Ghost ::
        (forall a n t. Dict (c n t a) -> String -> Proxy a -> b -> b) ->
        Proxy rts ->
        Proxy lts ->
        b ->
        b

instance RecApply3 rts '[] c where
    recApply3 _ _ _ b = b
    recApply3Ghost _ _ _ b = b

instance
    ( KnownSymbol l
    , RecApply3 rts (RemoveAccessTo l lts) c
    , Has l rts v
    , c n t v
    ) =>
    RecApply3 rts (l := t ': lts) c
    where
    recApply3 f r (_ :: Proxy (l := t ': lts)) b =
        let lbl :: FldProxy l
            lbl = FldProxy
            val = get lbl r
            res = f (Dict @(c n t v)) (symbolVal lbl) val b
            pNext :: Proxy (RemoveAccessTo l (l := t ': lts))
            pNext = Proxy
         in recApply3 f r pNext res

    recApply3Ghost f p (_ :: Proxy (l := t ': lts)) b =
        let lbl :: FldProxy l
            lbl = FldProxy
            res = f (Dict @(c n t v)) (symbolVal lbl) (Proxy :: Proxy v) b
            pNext :: Proxy (RemoveAccessTo l (l := t ': lts))
            pNext = Proxy
         in recApply3Ghost f p pNext res

reflectRec3Ghost ::
    forall c r lts.
    (RecApply3 lts lts c) =>
    Proxy c ->
    Proxy lts ->
    (forall a n t. c n t a => String -> r) ->
    [r]
reflectRec3Ghost _pc plts f  =
    reverse $
        recApply3Ghost
            (\(Dict :: Dict (c n t a)) s _p xs -> (f @a @n @t s : xs))
            plts
            (Proxy :: Proxy lts)
            []

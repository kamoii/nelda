{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module JRecExended where

import JRec
import JRec.Internal
import GHC.TypeLits (symbolVal, KnownSymbol)
import Data.Data (Proxy(Proxy))
import Data.Constraint (Dict(Dict))

-- | JRec に足りない機能を実装

-- | Machinery needed to implement 'reflectRec'
class RecApply' (rts :: [*]) (lts :: [*]) c where
    recApply'
        :: (forall a. Dict (c a) -> String -> a -> b -> b)
        -> Rec rts
        -> Proxy lts
        -> b
        -> b
    recApplyGhost
        :: (forall a. Dict (c a) -> String -> Proxy a -> b -> b)
        -> Proxy rts
        -> Proxy lts
        -> b
        -> b

instance RecApply' rts '[] c where
    recApply' _ _ _ b = b
    recApplyGhost _ _ _ b = b

instance
    ( KnownSymbol l,
      RecApply' rts (RemoveAccessTo l lts) c,
      Has l rts v,
      c v
    ) => RecApply' rts (l := t ': lts) c where
    recApply' f r (_ :: Proxy (l := t ': lts)) b =
        let lbl :: FldProxy l
            lbl = FldProxy
            val = get lbl r
            res = f Dict (symbolVal lbl) val b
            pNext :: Proxy (RemoveAccessTo l (l := t ': lts))
            pNext = Proxy
        in recApply' f r pNext res

    recApplyGhost f p (_ :: Proxy (l := t ': lts)) b =
        let lbl :: FldProxy l
            lbl = FldProxy
            res = f Dict (symbolVal lbl) (Proxy :: Proxy v) b
            pNext :: Proxy (RemoveAccessTo l (l := t ': lts))
            pNext = Proxy
        in recApplyGhost f p pNext res

reflectRecGhost
    :: forall c r lts
    . (RecApply' lts lts c)
    => Proxy c
    -> (forall a. c a => String -> Proxy a -> r )
    -> Proxy lts
    -> [r]
reflectRecGhost _ f p =
    reverse $ recApplyGhost
        (\(Dict :: Dict (c a)) s p' xs -> (f s p' : xs))
        p
        (Proxy :: Proxy lts)
        []

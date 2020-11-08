{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SQL.Row where

import Data.Constraint (Constraint)
import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Database.Nelda.SQL.Col (Col (One))
import Database.Nelda.SQL.Nullability (Nullability (NonNull))
import Database.Nelda.SQL.Types (UntypedCol (Untyped))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, type (+), type (-))
import qualified GHC.TypeLits as TL

-- | A database row. A row is a collection of one or more columns.
newtype Row s (n :: Nullability) row = Many [UntypedCol]

-- | CS はあくまで row の取り得る値の一つ(という立てまえ)
-- 別のモジュールに移すか？
data CS (cs :: [Type])

data C (n :: Nullability) (a :: Type)
data (name :: Symbol) :- (c :: Type)
infix 6 :-

data CProxy (name :: Symbol) = CProxy

instance (KnownSymbol name, name ~ name') => IsLabel name (CProxy name') where
    fromLabel = CProxy

emptyRow :: Row s 'NonNull (CS '[])
emptyRow = Many []

(!+) ::
    ( KnownSymbol name
    , NameDuplicationCheck name cs
    , Append cs '[name :- C n a] ~ cs'
    ) =>
    CProxy name ->
    Col s n a ->
    Row s 'NonNull (CS cs) ->
    Row s 'NonNull (CS cs')
(!+) _ (One x) (Many xs) = Many $ xs <> [Untyped x]
infixr 4 !+ -- lens の (.~), (%~) と合せた

-- drop
dropCol ::
    forall name i s cs cs'.
    ( KnownSymbol name
    , KnownNat i
    , NameInclusionCheck name cs
    , NameIndex name cs ~ i
    , Remove i cs ~ cs'
    ) =>
    CProxy name ->
    Row s 'NonNull (CS cs) ->
    Row s 'NonNull (CS cs')
dropCol _ (Many xs) =
    let i = fromIntegral $ natVal (Proxy @i)
     in Many $ take i xs <> drop (i + 1) xs

-- append
-- unoin(nameの重複許可,よしなにmerge)はあぶないので提供しない
-- (!<>) ::
--     (Append cs0 cs1 ~ cs2) =>
--     Row s 'NonNull (CS cs0) ->
--     Row s 'NonNull (CS cs1) ->
--     Row s 'NonNull (CS cs2)
-- (!<>) = undefined

type family Append (a :: [Type]) (b :: [Type]) :: [Type] where
    Append (a : a') b = a ': Append a' b
    Append '[] b = b

type family Remove (index :: Nat) (a :: [Type]) :: [Type] where
    Remove 0 (_ ': a') = a'
    Remove i (a ': a') = a ': Remove (i - 1) a'
    Remove _ '[] = '[]

type family NameDuplicationCheck (name :: Symbol) (cs :: [Type]) :: Constraint where
    NameDuplicationCheck name (name :- _ ': _) =
        TL.TypeError
            ( 'TL.Text "Name '"
                'TL.:<>: 'TL.Text name
                'TL.:<>: 'TL.Text "' already used."
            )
    NameDuplicationCheck name (_ ': cs') = NameDuplicationCheck name cs'
    NameDuplicationCheck _ '[] = ()

type family NameInclusionCheck (name :: Symbol) (cs :: [Type]) :: Constraint where
    NameInclusionCheck name (name :- _ ': _) = ()
    NameInclusionCheck name (_ ': cs') = NameInclusionCheck name cs'
    NameInclusionCheck name '[] =
        TL.TypeError
            ( 'TL.Text "Name '"
                'TL.:<>: 'TL.Text name
                'TL.:<>: 'TL.Text "' is not included."
            )

type NameIndex name cs = NameIndex' 0 name cs

-- TODO: constrint と Nat の '(c,i) 返すか？
type family NameIndex' (index :: Nat) (name :: Symbol) (cs :: [Type]) :: Nat where
    NameIndex' index name (name :- _ ': _) = index
    NameIndex' index name (_ :- _ ': cs') = NameIndex' (index + 1) name cs'
    NameIndex' _ _ '[] = 0 -- WOW. But `NameInclusionCheck' should save us

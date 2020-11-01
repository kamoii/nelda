{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Database.Nelda.SQL.Nullability where

-- * Nullability

data Nullability
    = NonNull
    | Nullable

-- * Nullability type family

-- TODO: Better opration name than (:.:) ....

-- type family (:.:) (n0 :: Nullability) (n1 :: Nullability) :: Nullability where
--     (:.:) 'NonNull 'NonNull = 'NonNull
--     (:.:) _ _ = 'Nullable

-- 一見上で上の定義で問題ないように見えるが,
-- ただこれは型の値が全部決まっていたらの場合。
-- 例えば (('NoNull :.: n) :.: n) は n と等しいはずなんだが,
-- 上の定義だと最初の  (:.:) 'NonNull 'NonNull = 'NonNull に
-- マッチするかどうか決められず計算が止まってしまう。
-- matchNull はそれで型注釈と 実際の型が無意味にずれてしまった。
-- 以下の形式にすることで (('NoNull :.: n) :.: n) が n まで計算される。

type family (:.:) (n0 :: Nullability) (n1 :: Nullability) :: Nullability where
    (:.:) n n = n
    (:.:) 'NonNull n = n
    (:.:) n 'NonNull = n
    (:.:) 'Nullable _ = 'Nullable
    (:.:) _ 'Nullable = 'Nullable

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Nelda.SQL.Col where

import Data.String (IsString (..))
import Data.Text (Text)
import Database.Nelda.SQL.Nullability
import Database.Nelda.SQL.Types
import Database.Nelda.SqlType (SqlType)
import qualified GHC.TypeLits as TL

-- | A database column. A column is often a literal column table, but can also
--   be an expression over such a column or a constant expression.
newtype Col s (n :: Nullability) a = One (Exp a)

-- | A literal expression.
-- NON NULL だが Nullability はコンテキスに決めてもらって問題ない
literal :: SqlType a => a -> Col s n a
literal = One . Lit . mkLit

nullLiteral :: SqlType a => Col s 'Nullable a
nullLiteral = One $ Lit $ mkNullLit

liftC3 ::
    (Exp a -> Exp b -> Exp c -> Exp d) ->
    Col s n0 a ->
    Col s n1 b ->
    Col s n2 c ->
    Col s n3 d
liftC3 f (One a) (One b) (One c) = One (f a b c)

liftC :: (Exp a -> Exp b) -> Col s n0 a -> Col s n1 b
liftC f (One x) = One (f x)

-- | Denotes that scopes @s@ and @t@ are identical.
class s ~ t => SameScope s t where
    liftC2 :: (Exp a -> Exp b -> Exp c) -> Col s n0 a -> Col t n1 b -> Col s n3 c
    liftC2 f (One a) (One b) = One (f a b)

instance {-# OVERLAPPING #-} SameScope s s
instance
    {-# OVERLAPPABLE #-}
    ( s ~ t
    , TL.TypeError
        ( 'TL.Text "An identifier from an outer scope may not be used in an inner query.")
    ) =>
    SameScope s t

instance (SqlType Text, n ~ 'NonNull) => IsString (Col s n Text) where
    fromString = literal . fromString

-- TODO: NonNull 制約に引っ掛るようなら .+, .* などの演算子を進めること
instance (SqlType a, Num a, n ~ 'NonNull) => Num (Col s n a) where
    fromInteger = literal . fromInteger
    (+) = liftC2 $ BinOp Add
    (-) = liftC2 $ BinOp Sub
    (*) = liftC2 $ BinOp Mul
    negate = liftC $ UnOp Neg
    abs = liftC $ UnOp Abs
    signum = liftC $ UnOp Sgn

instance (SqlType Double, n ~ 'NonNull) => Fractional (Col s n Double) where
    fromRational = literal . fromRational
    (/) = liftC2 $ BinOp Div

-- TODO: これはtype-safe SQL的にはあぶない気がする。
-- MySQL は select 5/3 -> 1.6667 になり Int にはならない(SQLite/Postgresだとなる)
-- haskell的には `div` とかでやったほうがいい気がする
-- instance (SqlType Int, n ~ 'NonNull) => Fractional (Col s n Int) where
--     fromRational = literal . (truncate :: Double -> Int) . fromRational
--     (/) = liftC2 $ BinOp Div

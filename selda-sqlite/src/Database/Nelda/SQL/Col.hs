{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Database.Nelda.SQL.Col where

import Database.Nelda.SQL.Types
import Database.Nelda.SqlType (SqlType)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified GHC.TypeLits as TL

-- | A database column. A column is often a literal column table, but can also
--   be an expression over such a column or a constant expression.
newtype Col s a = One (Exp a)

-- | A literal expression.
literal :: SqlType a => a -> Col s a
literal = One . Lit . mkLit

liftC3 :: (Exp a -> Exp b -> Exp c -> Exp d)
       -> Col s a
       -> Col s b
       -> Col s c
       -> Col s d
liftC3 f (One a) (One b) (One c) = One (f a b c)

liftC :: (Exp a -> Exp b) -> Col s a -> Col s b
liftC f (One x) = One (f x)

-- | Denotes that scopes @s@ and @t@ are identical.
class s ~ t => SameScope s t where
    liftC2 :: (Exp a -> Exp b -> Exp c) -> Col s a -> Col t b -> Col s c
    liftC2 f (One a) (One b) = One (f a b)

instance {-# OVERLAPPING #-} SameScope s s
instance {-# OVERLAPPABLE #-} (s ~ t, TL.TypeError
  ('TL.Text "An identifier from an outer scope may not be used in an inner query."))
  => SameScope s t

instance SqlType Text => IsString (Col s Text) where
  fromString = literal . fromString

instance (SqlType a, Num a) => Num (Col s a) where
    fromInteger = literal . fromInteger
    (+) = liftC2 $ BinOp Add
    (-) = liftC2 $ BinOp Sub
    (*) = liftC2 $ BinOp Mul
    negate = liftC $ UnOp Neg
    abs = liftC $ UnOp Abs
    signum = liftC $ UnOp Sgn

instance SqlType Double => Fractional (Col s Double) where
    fromRational = literal . fromRational
    (/) = liftC2 $ BinOp Div

instance SqlType Int => Fractional (Col s Int) where
    fromRational = literal . (truncate :: Double -> Int) . fromRational
    (/) = liftC2 $ BinOp Div

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.Query.SqlExpression where

import Database.Nelda.SQL.Col
import Database.Nelda.SQL.Types
import Database.Nelda.SqlType (SqlType)
import Database.Nelda.SqlOrd (SqlOrd)
import Database.Nelda.Query.Monad (Query)
import Data.Text (Text)
import Database.Nelda.SQL.Selector ((!), Selector)
import Database.Nelda.SQL.Row (Row)
import Database.Nelda.SQL.Aggr (liftAggr, aggr, Aggr)
import Unsafe.Coerce (unsafeCoerce)
import Database.Nelda.Query.SqlExpressionUnsafe (operator, fun)
import Database.Nelda.Compile.Query (compileQueryWithFreshScope)
import qualified Database.Nelda.Query.SqlExpressionUnsafe as Unsafe

-- Selda が Database.Selda.Nullable で提供していた Nullable 系の演算子は提供しない。
-- 片方の演算子 just すればいいだけの話なので。あまりにも面倒なら導入を考える。

-- TODO: SqlType a => a を取っており,かつ返り値が Bool とかになっていのは危ない...
-- NULL が混ると 結果も NULL になる可能性があるので...
-- SqlType type class に type IsNullable a :: 'Boolean 追加するか？
-- TODO: 先に失敗するケースかな？

-- * Compare Operation

-- | Comparisons over columns.
--   Note that when comparing nullable (i.e. @Maybe@) columns, SQL @NULL@
--   semantics are used. This means that comparing to a @NULL@ field will remove
--   the row in question from the current set.
--   To test for @NULL@, use 'isNull' instead of @.== literal Nothing@.
(.==), (./=) :: (SameScope s t, SqlType a) => Col s a -> Col t a -> Col s Bool
(.>), (.<), (.>=), (.<=) :: (SameScope s t, SqlOrd a) => Col s a -> Col t a -> Col s Bool
(.==) = liftC2 $ BinOp Eq
(./=) = liftC2 $ BinOp Neq
(.>)  = liftC2 $ BinOp Gt
(.<)  = liftC2 $ BinOp Lt
(.>=) = liftC2 $ BinOp Gte
(.<=) = liftC2 $ BinOp Lte
infixl 4 .==
infixl 4 ./=
infixl 4 .>
infixl 4 .<
infixl 4 .>=
infixl 4 .<=

-- * AND/OR

(.&&), (.||) :: SameScope s t => Col s Bool -> Col t Bool -> Col s Bool
(.&&) = liftC2 $ BinOp And
(.||) = liftC2 $ BinOp Or
infixr 3 .&&
infixr 2 .||

-- * IF THEN ELSE

-- | Perform a conditional on a column
ifThenElse :: (SameScope s t, SameScope t u, SqlType a) => Col s Bool -> Col t a -> Col u a -> Col s a
ifThenElse = liftC3 If

-- * IN

-- | Any container type for which we can check object membership.
class IsIn set where
    -- | Is the given column contained in the given set?
    isIn :: (SameScope s t, SqlType a) => Col s a -> set (Col t a) -> Col s Bool
infixl 4 `isIn`

instance IsIn [] where
    isIn _ []     = false
    isIn (One x) xs = One $ InList x [c | One c <- xs]

instance IsIn (Query s) where
    isIn (One x) = One . InQuery x . snd . compileQueryWithFreshScope

-- * LIKE

-- | The SQL @LIKE@ operator; matches strings with @%@ wildcards.
--   For instance:
--
-- > "%gon" `like` "dragon" .== true
like :: SameScope s t => Col s Text -> Col t Text -> Col s Bool
like = liftC2 $ BinOp Like
infixl 4 `like`

-- * NULL

-- | Is the given column null?
isNull :: SqlType a => Col s (Maybe a) -> Col s Bool
isNull = liftC $ UnOp IsNull

-- | Applies the given function to the given nullable column where it isn't null,
--   and returns the given default value where it is.
--
--   This is the Selda equivalent of 'maybe'.
matchNull
    :: (SqlType a, SqlType b, SameScope s t)
    => Col s b
    -> (Col s a -> Col s b)
    -> Col t (Maybe a)
    -> Col s b
matchNull nullvalue f x = ifThenElse (isNull x) nullvalue (f (Unsafe.fromNullable x))

-- | If the second value is Nothing, return the first value. Otherwise return
--   the second value.
ifNull :: (SameScope s t, SqlType a) => Col s a -> Col t (Maybe a) -> Col s a
ifNull nullvalue x = ifThenElse (isNull x) nullvalue (Unsafe.fromNullable x)

-- | Lift a non-nullable column to a nullable one.
--   Useful for creating expressions over optional columns:
-- > peopleWithCats = do
-- >   person <- select people
-- >   restrict (person ! #pet .== just "cat")
-- >   return (person ! #name)
--
-- TODO: これもともと cast で実装されてたけど不要のはずでおk?
just :: SqlType a => Col s a -> Col s (Maybe a)
just = unsafeCoerce


-- * Literal

-- | Returns 'true' if the given field in the given row is equal to the given
--   literal.
is :: forall r s c. SqlType c => Selector r c -> c -> Row s r -> Col s Bool
is s x r = r ! s .== (literal x :: Col s c)

-- | SQL NULL, at any type you like.
null_ :: SqlType a => Col s (Maybe a)
null_ = literal Nothing

-- | Specialization of 'literal' for integers.
int :: Int -> Col s Int
int = literal

-- | Specialization of 'literal' for doubles.
float :: Double -> Col s Double
float = literal

-- | Specialization of 'literal' for text.
text :: Text -> Col s Text
text = literal

-- | True and false boolean literals.
true, false :: Col s Bool
true = literal True
false = literal False

-- * Function

-- | Round a value to the nearest integer. Equivalent to @roundTo 0@.
--
-- TODO: MySQL の場合,ROUND の結果は INTEGER になってないか？？
round_ :: Col s Double -> Col s Double
round_ = Unsafe.fun "ROUND"

-- | Round a column to the given number of decimals places.
roundTo :: Col s Int -> Col s Double -> Col s Double
roundTo = flip $ Unsafe.fun2 "ROUND"

-- | Calculate the length of a string column.
length_ :: Col s Text -> Col s Int
length_ = Unsafe.fun "LENGTH"

-- | Boolean negation.
not_ :: Col s Bool -> Col s Bool
not_ = liftC $ UnOp Not

-- | Convert the given string to uppercase.
toUpper :: Col s Text -> Col s Text
toUpper = fun "UPPER"

-- | Convert the given string to lowercase.
toLower :: Col s Text -> Col s Text
toLower = fun "LOWER"

-- * Aggregation Function

-- | The number of non-null values in the given column.
count :: SqlType a => Col s a -> Aggr s Int
count = aggr "COUNT"

-- | The average of all values in the given column.
avg :: (SqlType a, Num a) => Col s a -> Aggr s (Maybe a)
avg = aggr "AVG"

-- | The greatest value in the given column. Texts are compared lexically.
max_ :: SqlOrd a => Col s a -> Aggr s (Maybe a)
max_ = aggr "MAX"

-- | The smallest value in the given column. Texts are compared lexically.
min_ :: SqlOrd a => Col s a -> Aggr s (Maybe a)
min_ = aggr "MIN"

-- | Sum all values in the given column.
sum_ :: forall a s. (SqlType a, Num a) => Col s a -> Aggr s a
sum_ = liftAggr (ifNull (0::Col s a)) . aggr "SUM"

-- * Casting(あかん)

-- 良くない。
--
-- -- | Convert a boolean column to any numeric type.
-- fromBool :: (SqlType a, Num a) => Col s Bool -> Col s a
--     fromBool = cast

-- -- | Convert an integer column to any numeric type.
-- fromInt :: (SqlType a, Num a) => Col s Int -> Col s a
-- fromInt = cast

-- -- | Convert any SQL type to a string.
-- toString :: SqlType a => Col s a -> Col s Text
-- toString = cast

-- * Mappable (CAST?)

-- -- | Any container type which can be mapped over.
-- --   Sort of like 'Functor', if you squint a bit.
-- class Mappable f where
--   type Container f a
--   (.<$>) :: (SqlType a, SqlType b)
--          => (Col s a -> Col s b)
--          -> f s (Container f a)
--          -> f s (Container f b)
-- infixl 4 .<$>
--
-- instance Mappable Aggr where
--   type Container Aggr a = a
--   (.<$>) = liftAggr
--
-- instance Mappable Col where
--   type Container Col a = Maybe a
--   f .<$> mx = cast (f (cast mx))

-- * Assignment(不要？)

-- これは RecordDotPressessor として与えてほうがいいかな？
-- そもそも要るのか？

-- -- | Add the given column to the column pointed to by the given selector.
-- (+=) :: (SqlType a, Num (Col s a)) => Selector t a -> Col s a -> Assignment s t
-- s += c = s $= (+ c)
-- infixl 2 +=
--
-- -- | Subtract the given column from the column pointed to by the given selector.
-- (-=) :: (SqlType a, Num (Col s a)) => Selector t a -> Col s a -> Assignment s t
-- s -= c = s $= (\x -> x - c)
-- infixl 2 -=
--
-- -- | Multiply the column pointed to by the given selector, by the given column.
-- (*=) :: (SqlType a, Num (Col s a)) => Selector t a -> Col s a -> Assignment s t
-- s *= c = s $= (* c)
-- infixl 2 *=
--
-- -- | Logically @OR@ the column pointed to by the given selector with
-- --   the given column.
-- (||=) :: Selector t Bool -> Col s Bool -> Assignment s t
-- s ||= c = s $= (.|| c)
-- infixl 2 ||=
--
-- -- | Logically @AND@ the column pointed to by the given selector with
-- --   the given column.
-- (&&=) :: Selector t Bool -> Col s Bool -> Assignment s t
-- s &&= c = s $= (.&& c)
-- infixl 2 &&=

-- * Orphan instances for Col s a

instance Semigroup (Col s Text) where
    (<>) = operator "||"
instance Monoid (Col s Text) where
    mempty = ""

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Nelda.Query.SqlExpression where

import Data.Text (Text)
import Database.Nelda.Compile.Query (compileQueryWithFreshScope)
import Database.Nelda.Query.Monad (Query)
import Database.Nelda.Query.Result (Result)
import Database.Nelda.Query.SqlExpressionUnsafe (fun, operator)
import qualified Database.Nelda.Query.SqlExpressionUnsafe as Unsafe
import Database.Nelda.SQL.Aggr (Aggr, aggr, liftAggr)
import Database.Nelda.SQL.Col
import Database.Nelda.SQL.Nullability
import Database.Nelda.SQL.Types
import Database.Nelda.SqlOrd (SqlOrd)
import Database.Nelda.SqlType (SqlType)
import Unsafe.Coerce (unsafeCoerce)

-- Selda が Database.Selda.Nullable で提供していた Nullable 系の演算子は提供しない。
-- 片方の演算子 just すればいいだけの話なので。あまりにも面倒なら導入を考える。

-- nullability には保守的に。
-- ひとつでも NULL が混れば基本結果も NULL でいいかな。

-- Nullability を forall n. n にしておくか 'NonNull にするか選択できる場合がある。
-- これはトレードオフ。forall n. n にしておけば 'Nullable なコンテキストでも利用できる。
-- ただし逆に n が定まらなくて明示的に指定しろ,と謂われると面倒なことになる。
--
-- -> そもそも 'Nullable を要求するコンテキストは基本ないはずなので,
-- NonNull が確定で分かる状況は NonNull と付けるべきという結論に。

-- * Compare Operation

-- | Comparisons over columns.
--   Note that when comparing nullable (i.e. @Maybe@) columns, SQL @NULL@
--   semantics are used. This means that comparing to a @NULL@ field will remove
--   the row in question from the current set.
--   To test for @NULL@, use 'isNull' instead of @.== literal Nothing@.
(.==), (./=) :: (SameScope s t, SqlType a) => Col s n0 a -> Col t n1 a -> Col s (n0 :.: n1) Bool
(.>), (.<), (.>=), (.<=) :: (SameScope s t, SqlOrd a) => Col s n0 a -> Col t n1 a -> Col s (n0 :.: n1) Bool
(.==) = liftC2 $ BinOp Eq
(./=) = liftC2 $ BinOp Neq

(.>) = liftC2 $ BinOp Gt
(.<) = liftC2 $ BinOp Lt
(.>=) = liftC2 $ BinOp Gte
(.<=) = liftC2 $ BinOp Lte
infixl 4 .==
infixl 4 ./=
infixl 4 .>
infixl 4 .<
infixl 4 .>=
infixl 4 .<=

-- * Numerical Operation

-- TODO: Num a, Factional a とは は雑かもしれない。ちゃんとするなら適切な型クラスを用意すべきかな

(.+), (.-), (.*) :: (SameScope s t, SqlType a, Num a) => Col s n0 a -> Col t n1 a -> Col s (n0 :.: n1) a
(.+) = liftC2 $ BinOp Add
(.-) = liftC2 $ BinOp Sub
(.*) = liftC2 $ BinOp Mul
infixl 6 .+
infixl 6 .-
infixl 7 .*

(./) ::
    (SameScope s t, SqlType a, Fractional a) =>
    Col s n0 a ->
    Col t n1 a ->
    Col s (n0 :.: n1) a
(./) = liftC2 $ BinOp Div
infixl 7 ./

-- 整数除算
-- SQLite, PostgreSQL だと / で整数除算になるが,MySQLだと DIVが必要。
div_ ::
    (SameScope s t, SqlType a, Integral a) =>
    Col s n0 a ->
    Col t n1 a ->
    Col s (n0 :.: n1) a
div_ = liftC2 $ BinOp IntDiv
infixl 7 `div_`

-- * AND/OR

(.&&), (.||) :: SameScope s t => Col s n0 Bool -> Col t n1 Bool -> Col s (n0 :.: n1) Bool
(.&&) = liftC2 $ BinOp And
(.||) = liftC2 $ BinOp Or
infixr 3 .&&
infixr 2 .||

-- * IF THEN ELSE

-- | Perform a conditional on a column
if_ ::
    (SameScope s t, SameScope t u, SqlType a) =>
    Col s n0 Bool ->
    Col t n1 a ->
    Col u n2 a ->
    Col s (n0 :.: n1 :.: n2) a
if_ = liftC3 If

-- * IN

-- MySQL では select 2 IN (1,NULL); が NULL になる。
-- なので n, n' のいずれか Nullable であれば 結果も Nullable とするのが安全かな。

-- | Any container type for which we can check object membership.
class IsIn set where
    -- | Is the given column contained in the given set?
    isIn_ ::
        (SameScope s t, SqlType a, Result (Col t n1 a) _r) =>
        Col s n0 a ->
        set (Col t n1 a) ->
        Col s (n0 :.: n1) Bool

infixl 4 `isIn_`

instance IsIn [] where
    isIn_ _ [] = Unsafe.liftNullability false_
    isIn_ (One x) xs = One $ InList x [c | One c <- xs]

instance IsIn (Query s) where
    isIn_ (One x) = One . InQuery x . snd . compileQueryWithFreshScope

-- * LIKE

-- | The SQL @LIKE@ operator; matches strings with @%@ wildcards.
--   For instance:
--
-- > "%gon" `like` "dragon" .== true
like_ :: SameScope s t => Col s n0 Text -> Col t n1 Text -> Col s (n0 :.: n1) Bool
like_ = liftC2 $ BinOp Like

infixl 4 `like_`

-- * NULL

-- | Is the given column null?
isNull_ :: SqlType a => Col s n a -> Col s 'NonNull Bool
isNull_ = liftC $ UnOp IsNull

-- | Applies the given function to the given nullable column where it isn't null,
--   and returns the given default value where it is.
--
--   This is the Selda equivalent of 'maybe'.
matchNull_ ::
    (SqlType a, SqlType b, SameScope s t) =>
    Col s n b ->
    (Col s 'NonNull a -> Col s n b) ->
    Col t 'Nullable a ->
    Col s n b
matchNull_ def f x = if_ (isNull_ x) def (f (Unsafe.fromNullable x))

-- | If the second value is Nothing, return the first value. Otherwise return
--   the second value.
ifNull_ :: (SameScope s t, SqlType a) => Col s n a -> Col t 'Nullable a -> Col s n a
ifNull_ nullvalue x = if_ (isNull_ x) nullvalue (Unsafe.fromNullable x)

-- | Lift a non-nullable column to a nullable one.
--   Useful for creating expressions over optional columns:
-- > peopleWithCats = do
-- >   person <- select people
-- >   restrict (person ! #pet .== just "cat")
-- >   return (person ! #name)
--
-- TODO: これもともと cast で実装されてたけど不要のはずでおk?
toNullable :: SqlType a => Col s n a -> Col s 'Nullable a
toNullable = unsafeCoerce

-- * Literal

-- RecordDotSyntas では使わないかな...
-- -- | Returns 'true' if the given field in the given row is equal to the given
-- --   literal.
-- is_ :: forall r s c. SqlType c => Selector r c -> c -> Row s r -> Col s Bool
-- is_ s x r = r ! s .== (literal x :: Col s c)

-- | SQL NULL, at any type you like.
-- e.g. null_ @Int
null_ :: forall a s. SqlType a => Col s 'Nullable a
null_ = nullLiteral

-- | Specialization of 'literal' for integers.
int_ :: Int -> Col s 'NonNull Int
int_ = literal

-- | Specialization of 'literal' for doubles.
float_ :: Double -> Col s 'NonNull Double
float_ = literal

-- | Specialization of 'literal' for text.
text_ :: Text -> Col s 'NonNull Text
text_ = literal

-- | True and false boolean literals.
true_, false_ :: Col s 'NonNull Bool
true_ = literal True
false_ = literal False

-- * Function

-- | Round a value to the nearest integer. Equivalent to @roundTo 0@.
--
-- TODO: MySQL の場合,ROUND の結果は INTEGER になってないか？？
round_ :: Col s n Double -> Col s n Double
round_ = Unsafe.fun "ROUND"

-- | Round a column to the given number of decimals places.
roundTo_ :: Col s n0 Int -> Col s n1 Double -> Col s (n0 :.: n1) Double
roundTo_ = flip $ Unsafe.fun2 "ROUND"

-- | Calculate the length of a string column.
length_ :: Col s n Text -> Col s n Int
length_ = Unsafe.fun "LENGTH"

-- | Boolean negation.
not_ :: Col s n Bool -> Col s n Bool
not_ = liftC $ UnOp Not

-- | Convert the given string to uppercase.
toUpper_ :: Col s n Text -> Col s n Text
toUpper_ = fun "UPPER"

-- | Convert the given string to lowercase.
toLower_ :: Col s n Text -> Col s n Text
toLower_ = fun "LOWER"

-- * Aggregation Function

-- | The number of non-null values in the given column.
-- Count result is always non-NULL. NULL values will be simply skipped so be carefull.
-- TODO: もし上記の挙動が DB全部で成り立つなら count_ は NonNUllだけ,count_' は Nullable も受け付けるようにしたほうがいいかな。
count_ :: SqlType a => Col s n a -> Aggr s 'NonNull Int
count_ = aggr "COUNT"

-- | The average of all values in the given column.
--   If the group has zero column, its
avg_ :: (SqlType a, Num a) => Col s _n a -> Aggr s 'Nullable a
avg_ = aggr "AVG"

-- | The greatest value in the given column. Texts are compared lexically.
max_ :: SqlOrd a => Col s _n a -> Aggr s 'Nullable a
max_ = aggr "MAX"

-- | The smallest value in the given column. Texts are compared lexically.
min_ :: SqlOrd a => Col s _n a -> Aggr s 'Nullable a
min_ = aggr "MIN"

-- | Sum all values in the given column.
sum_ :: forall a s n. (SqlType a, Num a) => Col s n a -> Aggr s 'NonNull a
sum_ = liftAggr (ifNull_ (0 :: Col s 'NonNull a)) . aggr "SUM"

-- * Orphan instances for Col s a

-- Nullabilty should't care

instance Semigroup (Col s n Text) where
    (<>) = operator "||"
instance Monoid (Col s n Text) where
    mempty = Unsafe.liftNullability ""

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Nelda.Query.SqlExpressionUnsafe where

import Data.Text (Text)
import Database.Nelda.SQL.Aggr (Aggr, liftAggr)
import Database.Nelda.SQL.Col (Col (One), liftC, liftC2)
import Database.Nelda.SQL.Nullability
import Database.Nelda.SQL.Scope (Inner)
import Database.Nelda.SQL.Types
import Database.Nelda.Schema (ColumnType (ColumnType))
import Database.Nelda.SqlType (SqlType)
import Unsafe.Coerce (unsafeCoerce)

-- 基本的に Unsafe.cast のように qualified モジュール付きで呼ぶこと。

-- 基本的に Maybe は ネストしないはずの前提
-- LeftCols や (?) が CoeleaseMaybe しているので。
fromNullable :: SqlType a => Col s 'Nullable a -> Col s 'NonNull a
fromNullable = unsafeCoerce

liftNullability :: SqlType a => Col s n a -> Col s n' a
liftNullability = unsafeCoerce

-- | Cast a column to another type, using whichever coercion semantics are used
--   by the underlying SQL implementation.
--
-- 例え事前定義された ColumnType を渡そうが,完全には安全
--
--  * 互換性のある型でも変換で情報が落ちる可能性がある
--  * 互換性のない型もある
--  * MySQL などはCAST可能な型の種類が制限されている上に型の表記方法がカラム定義と事なるものがある。
--    例えば INT UNSIGNED は CAST 時は UNSIGNED INT と表記する必要がある。
--
-- 型クラスを追加したり,メソッドを追加することでより型安全性を与えられるが,
-- そもそも CAST 自体あまり推奨されるものではないのでとりあえず Unsafeモジュールで提供。
--
-- References.
-- SQLite https://sqlite.org/lang_expr.html#castexpr
-- MySQL  https://dev.mysql.com/doc/refman/5.6/ja/cast-functions.html#function_cast
cast :: forall s ct st a n. SqlType st => ColumnType ct st -> Col s n a -> Col s n st
cast (ColumnType rep) = liftC $ Cast rep

-- | Cast an aggregate to another type, using whichever coercion semantics
--   are used by the underlying SQL implementation.
castAggr :: forall s ct st a n. SqlType st => ColumnType ct st -> Aggr s n a -> Aggr s n st
castAggr = liftAggr . cast

-- | Sink the given function into an inner scope.
--
--   Be careful not to use this function with functions capturing rows or columns
--   from an outer scope. For instance, the following usage will likely
--   lead to disaster:
--
-- > query $ do
-- >   x <- #age `from` select person
-- >   inner $ sink (\p -> x + (p ! #age)) <$> select person
--
--   Really, if you have to use this function, ONLY do so in the global scope.
sink :: (f s a -> f s b) -> f (Inner s) a -> f (Inner s) b
sink = unsafeCoerce

-- | Like 'sink', but with two arguments.
sink2 :: (f s a -> f s b -> f s c) -> f (Inner s) a -> f (Inner s) b -> f (Inner s) c
sink2 = unsafeCoerce

-- | A unary operation. Note that the provided function name is spliced
--   directly into the resulting SQL query. Thus, this function should ONLY
--   be used to implement well-defined functions that are missing from Selda's
--   standard library, and NOT in an ad hoc manner during queries.
fun :: Text -> Col s na a -> Col s nb b
fun = liftC . UnOp . Fun

-- | Like 'fun', but with zero arguments.
fun0 :: Text -> Col s n a
fun0 = One . NulOp . Fun0

-- | Like 'fun', but with two arguments.
fun2 :: Text -> Col s na a -> Col s nb b -> Col s nc c
fun2 = liftC2 . Fun2

-- | A custom operator. @operator "~>" a b@ will compile down to
--   @a ~> b@, with parentheses around @a@ and @b@ iff they are not atomic.
--   This means that SQL operator precedence is disregarded, as all
--   subexpressions are parenthesized. In the following example for instance,
--   @foo a b c@ will compile down to @(a ~> b) ~> c@.
--
-- > (~>) = operator "~>"
-- > infixl 5 ~>
-- > foo a b c = a ~> b ~> c
operator :: Text -> Col s na a -> Col s nb b -> Col s nc c
operator = liftC2 . BinOp . CustomOp

-- | Create a column referring to a name of your choice.
--   Use this to refer to variables not exposed by Selda.
rawName :: forall a n s. SqlType a => ColName -> Col s n a
rawName = One . Col

-- | Create an expression from the given text.
--   The expression will be inserted verbatim into your query, so you should
--   NEVER pass user-provided text to this function.
rawExp :: forall a n s. SqlType a => Text -> Col s n a
rawExp = One . Raw

-- -- | Execute a raw SQL statement.
-- rawStm :: MonadNelda m => QueryFragment -> m ()
-- rawStm q = withBackend $ \b -> liftIO $ do
--     let (sql, params) = compRaw Config.ppConfig q
--     void $ runStmt b sql (map paramToSqlParam params)
--

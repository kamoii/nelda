{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Nelda.Query.SqlExpressionUnsafe where

import Database.Nelda.SqlType (SqlType)
import Database.Nelda.SQL.Col (liftC, liftC2, Col(One), Col)
import Database.Nelda.SQL.Aggr (liftAggr, Aggr)
import Data.Text (Text)
import Database.Nelda.SQL.Types
import Unsafe.Coerce (unsafeCoerce)
import Database.Nelda.Schema (ColumnType(ColumnType))
import Database.Nelda.SQL.Scope (Inner)

-- 基本的に Unsafe.cast のように qualified モジュール付きで呼ぶこと。

-- 基本的に Maybe は ネストしないはずの前提
-- LeftCols や (?) が CoeleaseMaybe しているので。
fromNullable :: SqlType a => Col s (Maybe a) -> Col s a
fromNullable = unsafeCoerce

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
cast :: forall s ct st a. SqlType st => ColumnType ct st -> Col s a -> Col s st
cast (ColumnType rep) = liftC $ Cast rep

-- | Cast an aggregate to another type, using whichever coercion semantics
--   are used by the underlying SQL implementation.
castAggr :: forall s ct st a. SqlType st => ColumnType ct st -> Aggr s a -> Aggr s st
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
fun :: Text -> Col s a -> Col s b
fun = liftC . UnOp . Fun

-- | Like 'fun', but with two arguments.
fun2 :: Text -> Col s a -> Col s b -> Col s c
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
operator :: Text -> Col s a -> Col s b -> Col s c
operator = liftC2 . BinOp . CustomOp

-- | Like 'fun', but with zero arguments.
fun0 :: Text -> Col s a
fun0 = One . NulOp . Fun0

-- | Create a raw SQL query fragment from the given column.
inj :: Col s a -> QueryFragment
inj (One x) = RawExp x

-- | Create a raw SQL query fragment from the given value.
injLit :: SqlType a => a -> QueryFragment
injLit = RawExp . Lit . mkLit

-- | Create a column referring to a name of your choice.
--   Use this to refer to variables not exposed by Selda.
rawName :: SqlType a => ColName -> Col s a
rawName = One . Col

-- | Create an expression from the given text.
--   The expression will be inserted verbatim into your query, so you should
--   NEVER pass user-provided text to this function.
rawExp :: SqlType a => Text -> Col s a
rawExp = One . Raw

-- -- | Execute a raw SQL statement.
-- rawStm :: MonadNelda m => QueryFragment -> m ()
-- rawStm q = withBackend $ \b -> liftIO $ do
--     let (sql, params) = compRaw Config.ppConfig q
--     void $ runStmt b sql (map paramToSqlParam params)
--
-- -- | Execute a raw SQL statement, returning a row consisting of columns by the
-- --   given names.
-- --   Will fail if the number of names given does not match up with
-- --   the type of the returned row.
-- --   Will generate invalid SQL if the given names don't match up with the
-- --   column names in the given query.
-- rawQuery :: forall a s. SqlRow a => [ColName] -> QueryFragment -> Query s (Row s a)
-- rawQuery names q
--   | length names /= nestedCols (Proxy :: Proxy a) = do
--       let err = concat
--             [ "rawQuery: return type has ", show (nestedCols (Proxy :: Proxy a))
--             , " columns, but only ", show (length names), " names were given"
--             ]
--       throw (UnsafeError err)
--   | otherwise = Query $ do
--       rns <- renameAll [Untyped (Col name) | name <- names]
--       st <- get
--       put $ st { sources = sqlFrom rns (RawSql q) : sources st }
--       return (Many (map hideRenaming rns))
--
-- -- | As 'rawQuery', but returns only a single column. Same warnings still apply.
-- rawQuery1 :: SqlType a => ColName -> QueryFragment -> Query s (Col s a)
-- rawQuery1 name q = Query $ do
--   name' <- head <$> rename (Untyped (Col name))
--   st <- get
--   put $ st { sources = sqlFrom [name'] (RawSql q) : sources st }
--   case name' of
--     Named n _ -> return (One (Col n))
--     _         -> error "BUG: renaming did not rename"

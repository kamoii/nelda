{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Database.Nelda.Query.SqlClause where

import qualified Database.Nelda.Schema as Schema (ColumnName(..), Columns(..))
import Database.Nelda.Schema (Table(..), Column(..), ColumnNull(..), AnyColumn(..))
import Database.Nelda.Query.Monad (freshName, setSources, addSource, isolate, groupCols, staticRestricts, renameAll, sources, Query(..))
import Database.Nelda.SQL.Row (Row(Many), Row)
import Database.Nelda.SQL.Types
import Database.Nelda.SQL.Aggr (Aggr(..), unAggrs, Aggregates, LeftCols, AggrCols, Inner, OuterCols)
import Database.Nelda.SQL.Col (Col(One), SameScope)
import Database.Nelda.SqlType (SqlType)
import qualified Database.Nelda.SQL.Types as SQL
import Database.Nelda.Query.Columns (toTup, fromTup, Columns)

import Control.Monad.State.Strict (modify, get, put)
import JRec
import Database.Nelda.SQL.Transform (colNames, allCols, state2sql)
import Data.Maybe (isNothing)
import Unsafe.Coerce (unsafeCoerce)
import JRec.Internal (reflectRec, RecApply)
import Data.Data (Proxy(Proxy))
import JRecExended (reflectRecGhost, RecApply')
import Database.Nelda.SQL.Selector ((!))
import Database.Nelda.Query.SqlExpression (true)

-- * SELECT

type family QueryTypeColumnNullWrapping (nullabilty :: ColumnNull) (target :: *) :: * where
    QueryTypeColumnNullWrapping 'NotNull t = t
    QueryTypeColumnNullWrapping 'Nullable t = Maybe t
    QueryTypeColumnNullWrapping 'ImplicitNotNull t = t

type family ToQueryRecordField column :: * where
    ToQueryRecordField (Column name _ sqlType nullabilty _ _) =
        name := QueryTypeColumnNullWrapping nullabilty sqlType

type family ToQueryRecordFields columns :: [*] where
    ToQueryRecordFields '[] = '[]
    ToQueryRecordFields (column ': cs) = (ToQueryRecordField column ': ToQueryRecordFields cs)

select
    :: ( fields ~ ToQueryRecordFields cols )
    => Table name cols
    -> Query s (Row s (Rec fields))
select Table{tabName, tabColumns} = Query $ do
    rns <- renameAll $ columnsToUntypedCols tabColumns
    addSource $ sqlFrom rns (TableName tabName)
    pure $ Many (map hideRenaming rns)
  where
    columnsToUntypedCols (Schema.Columns anyColumns) =
        map (\(AnyColumn column) -> columnToUntypedCol column) anyColumns

    columnToUntypedCol Column{colName=Schema.ColumnName name} =
        Untyped $ Col $ mkColName name

-- * VALUES(pseudo)

{-
ad-hoc table/table literal

ちょっとした実験のときに便利。

PostgreSQL/SQLite は ad-hoc table として VALUES 句を持っている。
カラム名は column1, column2, .., columnN という名が自動的に振られる。
カラムに別名が付けられるのは PostgreSQLの別名リストを使ってのみ。
https://www.postgresql.jp/document/9.4/html/queries-values.html
https://sqlite.org/lang_select.html#values (6. The VALUES clause)

MySQL では残念ながらないらしい。
https://yoku0825.blogspot.com/2018/01/mysqlvalues.html
いや,MySQL 8.0.9 で追加されたらしい。
https://tmtms.hatenablog.com/entry/202001/mysql-table-values
カラム名は自動で column_N になる

ただ PostgreSQL/SQLite も実質は SELECT + UNION ALL と同等らしい。
SELECT 1 AS column1, 'one' AS column2
UNION ALL
SELECT 2, 'two'
UNION ALL
SELECT 3, 'three';

なのでとりあえずは SELECT + UNION ALL なのかな？
後空の場合の対応ってどうするかな...

元々の selda

selectValues ([] :: [Person])
-> ("SELECT $1, $2, NULL FROM (SELECT 1 FROM (SELECT NULL LIMIT 0) AS q0) AS q1",[Param "",Param 0])
selectValues [Person "Velvet" 19 (Just Dog),Person "Kobayashi" 23 (Just Dragon)]
-> ("SELECT \"tmp_0\", \"tmp_1\", \"tmp_2\" FROM (SELECT \"tmp_0\" AS \"tmp_0\", \"tmp_1\" AS \"tmp_1\", \"tmp_2\" AS \"tmp_2\" FROM (SELECT $1 AS \"tmp_0\", $2 AS \"tmp_1\", $3 AS \"tmp_2\" UNION ALL SELECT $4, $5, $6) AS q0) AS q1",[Param "Velvet",Param 19,Param "Dog",Param "Kobayashi",Param 23,Param "Dragon"])

空の場合特殊なことをしている。
空の defaultValue を使う gNew 使っているけど,これ全部 NULLでよくね？？？
値がある場合は SELECT + UNION ALL でやっている。

gNew
params

SqlType に type NullableType a :: * を追加するか？(Maybe だけ実装がsuru)
-}
-- | Query an ad hoc table of type @a@. Each element in the given list represents
--   one row in the ad hoc table.
-- TODO: RecApply と RecApply' を統合するか, JRec に PR
--
-- (1) a ~ Maybe Int の場合は Maybe (Maybe Int) となってしまうが,渡されるのは NULL なので問題はない...
values
    :: forall s lts
    . ( RecApply lts lts SqlType
      , RecApply' lts lts SqlType
      )
    => [Rec lts]
    -> Query s (Row s (Rec lts))
values [] = Query $ do
    addSource $ sqlFrom [] EmptyTable
    pure $ Many nullCols
  where
    nullCols = reflectRecGhost
      (Proxy :: Proxy SqlType)
      (\_ (_ :: Proxy a) -> Untyped $ Lit $ mkLit (Nothing :: (Maybe a))) -- (1)
      (Proxy :: Proxy lts)
values (row:rows) = Query $ do
    names <- mapM (const freshName) firstrow
    let rns  = [Named n (Col n) | n <- names]
    let row' = mkFirstRow names
    addSource $ sqlFrom rns (Values row' rows')
    pure $ Many (map hideRenaming rns)
  where
    toParams = reflectRec (Proxy :: Proxy SqlType) (\_ v -> param v)
    firstrow = toParams row
    mkFirstRow ns = [Named n (Lit l) | (Param l, n) <- zip firstrow ns]
    rows' = map toParams rows

-- TODO: 名前が微妙。rowValues/colValues にするか？
-- 別のモジュールに移すか？SqlUtils的な
colValues
    :: forall s a
    . SqlType a
    => [a]
    -> Query s (Col s a)
colValues vals = (! #tmp) <$> values (map (\a -> Rec (#tmp := a)) vals)

-- * UNION

_internalUnion
    :: (Columns a, Columns (OuterCols a))
    => Bool
    -> Query (Inner s) a
    -> Query (Inner s) a
    -> Query s (OuterCols a)
_internalUnion union_all a b = Query $ do
    (st_a, cols_a) <- isolate a
    (st_b, cols_b) <- isolate b
    renamed_a <- renameAll (fromTup cols_a)
    renamed_b <- renameAll (fromTup cols_b)
    let sql_a = (sqlFrom renamed_a (Product [state2sql st_a])) {liveExtras = renamed_a}
        sql_b = (sqlFrom renamed_b (Product [state2sql st_b])) {liveExtras = renamed_b}
        out_col_names = [n | Named n _ <- renamed_a]
        out_cols = map (Some . Col) out_col_names
    modify $ \st ->
        st {sources = sqlFrom out_cols (Union union_all sql_a sql_b) : sources st}
    return (toTup out_col_names)

-- | The set union of two queries. Equivalent to the SQL @UNION@ operator.
union
    :: (Columns a, Columns (OuterCols a))
    => Query (Inner s) a
    -> Query (Inner s) a
    -> Query s (OuterCols a)
union = _internalUnion False

-- | The multiset union of two queries.
--   Equivalent to the SQL @UNION ALL@ operator.
unionAll
    :: (Columns a, Columns (OuterCols a))
    => Query (Inner s) a
    -> Query (Inner s) a
    -> Query s (OuterCols a)
unionAll = _internalUnion True

-- * RESTRICT

-- | Restrict the query somehow. Roughly equivalent to @WHERE@.
restrict :: SameScope s t => Col s Bool -> Query t ()
restrict (One p) = Query $ do
    st <- get
    put $ case sources st of
      [] ->
        st {staticRestricts = p : staticRestricts st}
      -- PostgreSQL doesn't put renamed columns in scope in the WHERE clause
      -- of the query where they are renamed, so if the restrict predicate
      -- contains any vars renamed in this query, we must add another query
      -- just for the restrict.
      [sql] | not $ p `wasRenamedIn` cols sql ->
        st {sources = [sql {restricts = p : restricts sql}]}
      ss ->
        st {sources = [(sqlFrom (allCols ss) (Product ss)) {restricts = [p]}]}
  where
    wasRenamedIn predicate cs =
      let cs' = [n | Named n _ <- cs]
      in  any (`elem` cs') (colNames [Some predicate])

-- * AGGREGATE

-- | Execute a query, returning an aggregation of its results.
--   The query must return an inductive tuple of 'Aggregate' columns.
--   When @aggregate@ returns, those columns are converted into non-aggregate
--   columns, which may then be used to further restrict the query.
--
--   Note that aggregate queries must not depend on outer queries, nor must
--   they return any non-aggregate columns. Attempting to do either results in
--   a type error.
--
--   The SQL @HAVING@ keyword can be implemented by combining @aggregate@
--   and 'restrict':
--
-- > -- Find the number of people living on every address, for all addresses
-- > -- with more than one tenant:
-- > -- SELECT COUNT(name) AS c, address FROM housing GROUP BY name HAVING c > 1
-- >
-- > numPpl = do
-- >   (num_tenants :*: theAddress) <- aggregate $ do
-- >     h <- select housing
-- >     theAddress <- groupBy (h ! address)
-- >     return (count (h ! address) :*: theAddress)
-- >  restrict (num_tenants .> 1)
-- >  return (num_tenants :*: theAddress)
aggregate
    :: (Columns (AggrCols a), Aggregates a)
    => Query (Inner s) a
    -> Query s (AggrCols a)
aggregate q = Query $ do
    (gst, aggrs) <- isolate q
    cs <- renameAll $ unAggrs aggrs
    let sql = (sqlFrom cs (Product [state2sql gst])) {groups = groupCols gst}
    addSource sql
    pure $ toTup [n | Named n _ <- cs]

-- * JOINING

-- | Perform a @LEFT JOIN@ with the current result set (i.e. the outer query)
--   as the left hand side, and the given query as the right hand side.
--   Like with 'aggregate', the inner (or right) query must not depend on the
--   outer (or right) one.
--
--   The given predicate over the values returned by the inner query determines
--   for each row whether to join or not. This predicate may depend on any
--   values from the outer query.
--
--   For instance, the following will list everyone in the @people@ table
--   together with their address if they have one; if they don't, the address
--   field will be @NULL@.
--
-- > getAddresses :: Query s (Col s Text :*: Col s (Maybe Text))
-- > getAddresses = do
-- >   (name :*: _) <- select people
-- >   (_ :*: address) <- leftJoin (\(n :*: _) -> n .== name)
-- >                               (select addresses)
-- >   return (name :*: address)
leftJoin
    :: (Columns a, Columns (OuterCols a), Columns (LeftCols a))
    => (OuterCols a -> Col s Bool)
      -- ^ Predicate determining which lines to join.
      -- | Right-hand query to join.
    -> Query (Inner s) a
    -> Query s (LeftCols a)
leftJoin = someJoin LeftJoin

-- | Perform an @INNER JOIN@ with the current result set and the given query.
innerJoin
    :: (Columns a, Columns (OuterCols a))
    => (OuterCols a -> Col s Bool)
      -- ^ Predicate determining which lines to join.
      -- | Right-hand query to join.
    -> Query (Inner s) a
    -> Query s (OuterCols a)
innerJoin = someJoin InnerJoin

-- | The actual code for any join.
someJoin
    :: (Columns a, Columns (OuterCols a), Columns a')
    => JoinType
    -> (OuterCols a -> Col s Bool)
    -> Query (Inner s) a
    -> Query s a'
someJoin jointype check q = Query $ do
  (join_st, res) <- isolate q
  cs <- renameAll $ fromTup res
  st <- get
  let nameds = [n | Named n _ <- cs]
      left = state2sql st
      right = sqlFrom cs (Product [state2sql join_st])
      One on = check $ toTup nameds
      outCols = [Some $ Col n | Named n _ <- cs] ++ allCols [left]
  put $ st {sources = [sqlFrom outCols (Join jointype on left right)]}
  pure $ toTup nameds

-- * inner/suchThat utility (必要かどうか判断中)

-- | Explicitly create an inner query. Equivalent to @innerJoin (const true)@.
--
--   Sometimes it's handy, for performance
--   reasons and otherwise, to perform a subquery and restrict only that query
--   before adding the result of the query to the result set, instead of first
--   adding the query to the result set and restricting the whole result set
--   afterwards.
inner
    :: (Columns a, Columns (OuterCols a))
    => Query (Inner s) a
    -> Query s (OuterCols a)
inner = innerJoin (const true)

-- | Create and filter an inner query, before adding it to the current result
--   set.
--
--   @q `suchThat` p@ is generally more efficient than
--   @select q >>= \x -> restrict (p x) >> pure x@.
suchThat
    :: (Columns a, Columns (OuterCols a))
    => Query (Inner s) a
    -> (a -> Col (Inner s) Bool)
    -> Query s (OuterCols a)
suchThat q p = inner $ do
    x <- q
    restrict (p x)
    return x

infixr 7 `suchThat`

-- * GROUP BY

-- | Group an aggregate query by a column.
--   Attempting to group a non-aggregate query is a type error.
--   An aggregate representing the grouped-by column is returned, which can be
--   returned from the aggregate query. For instance, if you want to find out
--   how many people have a pet at home:
--
-- > aggregate $ do
-- >   person <- select people
-- >   name' <- groupBy (person ! name)
-- >   return (name' :*: count(person ! pet_name) .> 0)
groupBy :: (SameScope s t, SqlType a) => Col (Inner s) a -> Query (Inner t) (Aggr (Inner t) a)
groupBy (One c) = Query $ do
    st <- get
    put $ st {groupCols = Some c : groupCols st}
    return (Aggr c)

-- * LIMIT

-- | Drop the first @m@ rows, then get at most @n@ of the remaining rows from the
--   given subquery.
limit :: SameScope s t => Int -> Int -> Query (Inner s) a -> Query t (OuterCols a)
limit from to q = Query $ do
    (lim_st, res) <- isolate q
    let sql' = case sources lim_st of
            [sql] | isNothing (limits sql) -> sql
            ss                             -> sqlFrom (allCols ss) (Product ss)
    addSource $ sql' {limits = Just (from, to)}
    pure $ unsafeCoerce res

-- * ORDER BY

-- | Sort the result rows in ascending or descending order on the given row.
--
--   If multiple @order@ directives are given, later directives are given
--   precedence but do not cancel out earlier ordering directives.
--   To get a list of persons sorted primarily on age and secondarily on name:
--
-- > peopleInAgeAndNameOrder = do
-- >   person <- select people
-- >   order (person ! name) ascending
-- >   order (person ! age) ascending
-- >   return (person ! name)
--
--   For a table @[("Alice", 20), ("Bob", 20), ("Eve", 18)]@, this query
--   will always return @["Eve", "Alice", "Bob"]@.
--
--   The reason for later orderings taking precedence and not the other way
--   around is composability: @order@ should always sort the current
--   result set to avoid weird surprises when a previous @order@ directive
--   is buried somewhere deep in an earlier query.
--   However, the ordering must always be stable, to ensure that previous
--   calls to order are not simply erased.
order :: (SameScope s t, SqlType a) => Col s a -> Order -> Query t ()
order (One c) o = Query $ do
    st <- get
    case sources st of
        [sql] -> put st {sources = [sql {ordering = (o, Some c) : ordering sql}]}
        ss    -> put st {sources = [sql {ordering = [(o, Some c)]}]}
          where sql = sqlFrom (allCols ss) (Product ss)

-- | Ordering for 'order'.
ascending, descending :: Order
ascending = Asc
descending = Desc

-- | Sort the result rows in random order.
orderRandom :: Query s ()
orderRandom = order (One (NulOp (Fun0 "RANDOM") :: Exp Int)) Asc

-- * DISTINCT

-- | Remove all duplicates from the result set.
distinct
    :: (Columns a, Columns (OuterCols a))
    => Query (Inner s) a
    -> Query s (OuterCols a)
distinct q = Query $ do
    (inner_st, res) <- isolate q
    let ss = sources inner_st
    setSources [(sqlFrom (allCols ss) (Product ss)) {SQL.distinct = True}]
    pure $ unsafeCoerce res

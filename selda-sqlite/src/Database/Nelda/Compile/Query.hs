module Database.Nelda.Compile.Query where

import Database.Nelda.Types (Sql)
import Database.Nelda.Query.Result (finalCols, Result)
import Database.Nelda.Query.Name (Scope)
import Database.Nelda.Query.Monad (runQueryM, nameSupply, Query)
import Database.Nelda.SQL.Types (Param, SqlSource(Product), SQL(SQL))
import Database.Nelda.SQL.Transform (removeDeadCols, implicitlyLiveCols, colNames, state2sql)
import Database.Nelda.Compile.SQL (compileSQL)

-- | Compile a query into a parameterised SQL statement.
--
--   The types given are tailored for SQLite. To translate SQLite types into
--   whichever types are used by your backend, use 'compileWith'.
compileQuery :: Result a => Query s a -> (Sql, [Param])
compileQuery = compileSQL . snd . _compileQuery 0

-- | Compile a query to an SQL AST.
--   Groups are ignored, as they are only used by 'aggregate'.
_compileQuery :: Result a => Scope -> Query s a -> (Int, SQL)
_compileQuery ns q =
    (nameSupply st, SQL final (Product [srcs]) [] [] [] Nothing [] False)
  where
    (cs, st) = runQueryM ns q
    final = finalCols cs
    sql = state2sql st
    live = colNames final ++ implicitlyLiveCols sql
    srcs = removeDeadCols live sql

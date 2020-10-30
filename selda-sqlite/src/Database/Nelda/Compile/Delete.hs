{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Nelda.Compile.Delete where

import Data.Text (Text)
import qualified Data.Text as Text
import Database.Nelda.Compile.Quoting (quoteTableName)
import Database.Nelda.Compile.TableFields (ToQueryFields)
import Database.Nelda.SQL.Col (Col (One))
import Database.Nelda.SQL.Row (Row)
import Database.Nelda.SQL.Types (Exp)
import Database.Nelda.Schema (Table (..), TableName)
import Database.Nelda.SqlType (SqlParam)
import Database.Nelda.Types (Sql (Sql))
import JRec

-- -- | Compile a @DELETE FROM@ query.
-- --
-- -- TODO: toTup は何をしている?必要なのか？
-- compileDelete ::
--     (lts ~ ToQueryFields cols) =>
--     Table name cols ->
--     (Row s (Rec lts) -> Col s Bool) ->
--     (Sql, [SqlParam])
-- compileDelete Table{tabName, tabColumns} check = (Sql statement, params)
--   where
--     (statement, params) = unsafeCompileDelete tabName predicate
--     One predicate = check $ toTup $ map colName tabColumns
--
-- -- | Compile a @DELETE@ statement.
-- unsafeCompileDelete :: TableName -> Exp Bool -> (Text, [SqlParam])
-- unsafeCompileDelete tbl pred = runPP ppDelete
--   where
--     ppDelete = do
--         c' <- ppCol pred
--         pure $ Text.unwords ["DELETE FROM", quoteTableName tbl, "WHERE", c']

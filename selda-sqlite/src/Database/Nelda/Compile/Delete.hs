module Database.Nelda.Compile.Delete where

-- | Compile a @DELETE FROM@ query.
compileDelete ::
    Table name cols ->
    (Row s a -> Col s Bool) ->
    (Text, [Param])
compileDelete tbl check = compDelete cfg (tableName tbl) predicate
  where
    One predicate = check $ toTup $ map colName $ tableCols tbl

-- | Compile a @DELETE@ statement.
unsafeCompileDelete ::  TableName -> Exp Bool -> (Text, [Param])
unsafeCompileDelete cfg tbl p = runPP cfg ppDelete
  where
    ppDelete = do
        c' <- ppCol p
        pure $ Text.unwords ["DELETE FROM", fromTableName tbl, "WHERE", c']

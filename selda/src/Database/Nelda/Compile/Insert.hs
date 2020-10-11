{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Database.Nelda.Compile.Insert where

import Database.Nelda.Types (Sql)
import Database.Nelda.Schema (Table(..), Column(..), Nullability(..), Columns(..), AnyColumn(..))

import qualified Database.Selda.Backend.PPConfig as PPConfig (ppMaxInsertParams)
import Database.Selda.Backend.Types (SqlParam)

import JRec

-- TODO: Selda からのコードから分離する。
-- TODO: AutoIncrement なフィールドを追加してくれる JRec ヘルパー関数があると便利かも

-- | Convert Table columns type to jrec's Rec fields.
type family ToInsertRecordField column :: * where
    ToInsertRecordField (Column name _ sqlType 'NotNull _) = name := sqlType
    ToInsertRecordField (Column name _ sqlType 'Nullable _) = name := Maybe sqlType

type family ToInsertRecordFields columns :: [*] where
    ToInsertRecordFields '[] = '[]
    ToInsertRecordFields (column ': cs) = (ToInsertRecordField column ': ToInsertRecordFields cs)

{-

Rec ("name" := "foo", "pet" := Maybe "dog", "hoge" := UseDefault, "bar" := Specify 4 )

-}
compileInsert
    :: ( fields ~ ToInsertRecordFields cols )
    => Table name cols
    -> [Rec fields]
    -> [(Sql, [SqlParam])]
compileInsert _ [] =
    [(mempty, [])]
compileInsert tbl rows =
    case PPConfig.ppMaxInsertParams of
        Nothing -> [_compileInsert tbl rows']
        Just n  -> map (_compileInsert tbl) (chunk (n `div` rowlen) rows')
  where
    rows' = map params rows
    rowlen = length (head rows')
    chunk chunksize xs =
        case splitAt chunksize xs of
            ([], []) -> []
            (x, [])  -> [x]
            (x, xs') -> x : chunk chunksize xs'

_compileInsert
    :: Table name cols
    -> [[Either Param Param]]
    -> (Sql, [SqlParam])
_compileInsert = undefined

-- | Compile an @INSERT INTO@ query inserting @m@ rows with @n@ cols each.
--   Note that backends expect insertions to NOT have a semicolon at the end.
--   In addition to the compiled query, this function also returns the list of
--   parameters to be passed to the backend.
-- compInsert :: PPConfig -> Table a -> [[Either Param Param]] -> (Text, [Param])
-- compInsert cfg tbl defs =
--     (query, parameters)
--   where
--     colNames = map colName $ tableCols tbl
--     values = Text.intercalate ", " vals
--     (vals, parameters) = mkRows 1 defs [] []
--     query = Text.unwords
--       [ "INSERT INTO"
--       , fromTableName (tableName tbl)
--       , "(" <>  Text.intercalate ", " (map fromColName colNames) <> ")"
--       , "VALUES"
--       , values
--       ]
--
--     -- Build all rows: just recurse over the list of defaults (which encodes
--     -- the # of elements in total as well), building each row, keeping track
--     -- of the next parameter identifier.
--     mkRows n (ps:pss) rts paramss =
--       case mkRow n ps (tableCols tbl) of
--         (n', names, params) -> mkRows n' pss (rowText:rts) (params:paramss)
--           where rowText = "(" <> Text.intercalate ", " (reverse names) <> ")"
--     mkRows _ _ rts ps =
--       (reverse rts, reverse $ concat ps)
--
--     -- Build a row: use the NULL/DEFAULT keyword for default rows, otherwise
--     -- use a parameter.
--     mkRow n ps names = foldl' mkCols (n, [], []) (zip ps names)
--
--     -- Build a column: default values only available for for auto-incrementing
--     -- primary keys.
--     mkCol :: Int -> Either Param Param -> ColInfo -> [Param] -> (Int, Text, [Param])
--     mkCol n (Left def) col ps
--       | any isAutoPrimary (colAttrs col) =
--         (n, ppAutoIncInsert cfg, ps)
--       | otherwise =
--         (n+1, pack ('$':show n), def:ps)
--     mkCol n (Right val) _ ps =
--         (n+1, pack ('$':show n), val:ps)
--
--     -- Create a colum and return the next parameter id, plus the column itself.
--     mkCols :: (Int, [Text], [Param]) -> (Either Param Param, ColInfo) -> (Int, [Text], [Param])
--     mkCols (n, names, params) (param, col) =
--       case mkCol n param col params of
--         (n', name, params') -> (n', name:names, params')

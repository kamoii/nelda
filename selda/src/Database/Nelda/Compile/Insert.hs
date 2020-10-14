{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Nelda.Compile.Insert where

import Database.Nelda.Types (Sql(..))
import Database.Nelda.Schema (Table(..), Column(..), Nullability(..), TableName(..))
import Database.Nelda.SqlType (SqlParam, SqlType(..))

-- import qualified Database.Selda.Backend.PPConfig as PPConfig (ppMaxInsertParams)

import Data.Proxy (Proxy(..))
import Data.Maybe (catMaybes)
import Data.Function ((&))
import qualified Data.Text as Text
import JRec
import JRec.Internal (reflectRec, RecApply)

-- TODO: Selda からのコードから分離する。
-- TODO: AutoIncrement なフィールドを追加してくれる JRec ヘルパー関数があると便利かも

-- | Convert Table columns type to jrec's Rec fields.

-- TODO: Defaultable s 対応
type family ToInsertRecordField column :: * where
    ToInsertRecordField (Column name _ sqlType 'NotNull _) = name := sqlType
    ToInsertRecordField (Column name _ sqlType 'Nullable _) = name := Maybe sqlType

type family ToInsertRecordFields columns :: [*] where
    ToInsertRecordFields '[] = '[]
    ToInsertRecordFields (column ': cs) = (ToInsertRecordField column ': ToInsertRecordFields cs)

-- To trigger AUTO INCREMENT or DEFAULT value
-- Defaultable は SqlValue である必要はない
-- TODO: ただしい定義場所へ還すのだ
data Defaultable a
    = UseDefault
    | Specify a
    deriving (Eq, Show)

data InsertSqlParam
    = ISPUseDefault
    | ISPSqlParam SqlParam

class ToInsretSqlParam v where
    _toInsretSqlParam :: v -> InsertSqlParam

instance SqlType a => ToInsretSqlParam (Defaultable a) where
    _toInsretSqlParam UseDefault = ISPUseDefault
    _toInsretSqlParam (Specify v) = ISPSqlParam $ toSqlParam v

instance {-# OVERLAPPABLE #-} SqlType v => ToInsretSqlParam v where
    _toInsretSqlParam = ISPSqlParam . toSqlParam

{-
ToInsertRecordFields, RecApply を直截使うとユーザが見えるところで微妙なので
e.g.
compileInsert
    :: ( fields ~ ToInsertRecordFields cols
      , RecApply fields fields ToInsretSqlParam
      )
    => Table name cols

型クラスを一つ挟む。
-}

class
    ( RecApply (InsertRecordFields t) (InsertRecordFields t) ToInsretSqlParam
    ) => InsertableTable t where
    type InsertRecordFields t :: [*]

instance
    ( RecApply (InsertRecordFields (Table name cols)) (InsertRecordFields (Table name cols)) ToInsretSqlParam
    ) => InsertableTable (Table name cols) where
    type InsertRecordFields (Table name cols) = ToInsertRecordFields cols

{-
Rec ("name" := "foo", "pet" := Maybe "dog", "hoge" := UseDefault, "bar" := Specify 4 )
-}

-- よしなに
compileInsert
    :: InsertableTable (Table name cols)
    => Table name cols
    -> [Rec (InsertRecordFields (Table name cols))]
    -> [(Sql, [SqlParam])]
compileInsert _ [] =
    [ (mempty, []) ]
compileInsert table [row] =
    [ compileInsertSingle table row ]
compileInsert table rows =
    -- TODO: compileInsertBatch をちゃんと定義して使う
    map (compileInsertSingle table) rows

-- TODO: compileInsertSingle
-- TODO: なぜ Single と Batch で実装を分けているか説明(SQLite におけるDefault のせい)
compileInsertSingle
    :: InsertableTable (Table name cols)
    => Table name cols
    -> Rec (InsertRecordFields (Table name cols))
    -> (Sql, [SqlParam])
compileInsertSingle Table{tabName} row = (sql, params)
    where
      -- TODO: Text ではなくて, SqlFragment のほうがいいかな?
      -- ただ SqlFragment もそこまで恩恵はないかな...
      sql = Sql $ Text.unwords
          [ "INSERT INTO"
          , quoteTableName tabName
          , "(" <>  Text.intercalate ", " names <> ")"
          , "VALUES"
          , "(" <>  Text.intercalate ", " placeholders <> ")"
          ]

      -- TODO: quoting はDB毎かな？
      -- https://www.prisma.io/dataguide/postgresql/short-guides/quoting-rules#single-quotes
      -- postgres だと quoating によって case sensitive か insesitive が変わる。
      -- といういか quote ルールは DB ごとなら .hsig ないで
      -- https://stackoverflow.com/questions/11004768/escape-table-name-mysql
      -- mysql の場合は ` か " (ただし " はオプションを有効にする必要あり)
      quoteTableName :: TableName a -> Text.Text
      quoteTableName (TableName name) = mconcat ["\"", escapeQuotes name, "\""]
        where
          escapeQuotes = Text.replace "\"" "\"\""

      names = map (Text.pack . fst) colsWithParam
      params = map snd colsWithParam
      placeholders = zip [1..] colsWithParam & map (\(i, _) -> Text.pack ('$' : show (i :: Int)))

      colsWithParam :: [(String, SqlParam)]
      colsWithParam = catMaybes $ map (traverse toMaybe) colsAll

      colsAll :: [(String, InsertSqlParam)]
      colsAll = reflectRec
          (Proxy :: Proxy ToInsretSqlParam)
          (\s v -> (s, _toInsretSqlParam v))
          row

      toMaybe ISPUseDefault = Nothing
      toMaybe (ISPSqlParam p) = Just p


-- TODO: compileInsertBatch

-- compileInsert
--     :: ( fields ~ ToInsertRecordFields cols )
--     => Table name cols
--     -> [Rec fields]
--     -> [(Sql, [SqlParam])]
-- compileInsert _ [] =
--     [(mempty, [])]
-- compileInsert tbl rows =
--     case PPConfig.ppMaxInsertParams of
--         Nothing -> [_compileInsert tbl rows']
--         Just n  -> map (_compileInsert tbl) (chunk (n `div` rowlen) rows')
--   where
--     rows' = map params rows
--     rowlen = length (head rows')
--     chunk chunksize xs =
--         case splitAt chunksize xs of
--             ([], []) -> []
--             (x, [])  -> [x]
--             (x, xs') -> x : chunk chunksize xs'
--
-- _compileInsert
--     :: Table name cols
--     -> [[Either Param Param]]
--     -> (Sql, [SqlParam])
-- _compileInsert = undefined

{-

ppAutoIncInsert って要らなくね？あー,いるか
-}
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

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
import Database.Nelda.Schema (Table(..), Column(..), Nullability(..), Default(..), TableName(..))
import Database.Nelda.SqlType (SqlParam, SqlType(..))

-- import qualified Database.Selda.Backend.PPConfig as PPConfig (ppMaxInsertParams)

import Data.Proxy (Proxy(..))
import Data.Maybe (catMaybes)
import Data.Function ((&))
import qualified Data.Text as Text
import JRec
import JRec.Internal (reflectRec, RecApply)

-- * Defaultable/AutoIncrement data type
--
-- Data type to specify columns which has explicit DEFAULT value or AUTO INCREMENT attribute.
-- いずれも Maybe 型と isomorphic だが semantics が異なるための別の型として定義している。
-- 現状 一つのカラムが explicit DEFAULT value と AUTO INCREMENT attribute を同時に持つことはないと仮定している。
-- つまり AutoIncrement (Defaultable Int) みたいなINSERT型はないはず(ToInsertRecordFieldの実装参照)。
-- そのため一つのデータ型で済むはずではあるが,性質を変えて扱いたい場合があるので別々に定義。
--
-- InsertableTable 型クラスを使う限り,ユーザがこれらの型を使うことは稀のはずである。
-- なので型名やコンストラクタが長ったらしくなってもまーいいかな。
--
-- TODO: 定義場所正しいのか？
-- TODO: Defaultable is an awful name..
data Defaultable a
    = UseDefault
    | IgnoreDefaultAndSpecify a
    deriving (Eq, Show)

data AutoIncrement a
    = TriggerAutoIncrement
    | IgnoreAutIncrementAndSpecify a
    deriving (Eq, Show)

-- * ToInsretSqlParam type class/instance: JRec record field to InsertSqlParam
--
-- InsertSqlParam に変換する際は DEFAULT も AUTO INCREMENT も一緒くたに扱っている。
-- TODO: これは別個にする必要があるかも

data InsertSqlParam
    = ISPUseDefault
    | ISPSqlParam SqlParam

class ToInsretSqlParam v where
    _toInsretSqlParam :: v -> InsertSqlParam

instance SqlType a => ToInsretSqlParam (Defaultable a) where
    _toInsretSqlParam UseDefault = ISPUseDefault
    _toInsretSqlParam (IgnoreDefaultAndSpecify v) = ISPSqlParam $ toSqlParam v

instance SqlType a => ToInsretSqlParam (AutoIncrement a) where
    _toInsretSqlParam TriggerAutoIncrement = ISPUseDefault
    _toInsretSqlParam (IgnoreAutIncrementAndSpecify v) = ISPSqlParam $ toSqlParam v

instance {-# OVERLAPPABLE #-} SqlType v => ToInsretSqlParam v where
    _toInsretSqlParam = ISPSqlParam . toSqlParam

-- * InsertableTable' type class/instance
--
-- Table created by table functions should always satisfiy InsertableTable' constraint.
-- InsertRecordFields type family で table に insert可能な完全な field list が得られる。
-- Field list の各型はいかのいずれかである。 st を Column の sqlType と ~ とする。
--
--  * st                       Not NULL かつ DEFAULT/AUTO INCREMENT ではない場合
--  * Maybe st                 NULL許容 かつ DEFAULT/AUTO INCREMENT ではない場合
--  * Defaultable st           Not NULL かつ DEFAULT/AUTO INCREMENT のいずれか
--  * Defaultable (Maybe st)   NULL許容 かつ DEFAULT/AUTO INCREMENT のいずれか

class
    ( RecApply (InsertRecordFields t) (InsertRecordFields t) ToInsretSqlParam
    ) => InsertableTable' t where
    type InsertRecordFields t :: [*]

instance
    ( RecApply (InsertRecordFields (Table name cols)) (InsertRecordFields (Table name cols)) ToInsretSqlParam
    ) => InsertableTable' (Table name cols) where
    type InsertRecordFields (Table name cols) = ToInsertRecordFields cols

type family ToInsertRecordFields columns :: [*] where
    ToInsertRecordFields '[] = '[]
    ToInsertRecordFields (column ': cs) = (ToInsertRecordField column ': ToInsertRecordFields cs)

type family ToInsertRecordField column :: * where
    ToInsertRecordField (Column name _ sqlType 'NotNull 'NoDefault)  = name := sqlType
    ToInsertRecordField (Column name _ sqlType 'NotNull _)           = name := Defaultable sqlType
    ToInsertRecordField (Column name _ sqlType 'Nullable 'NoDefault) = name := Maybe sqlType
    ToInsertRecordField (Column name _ sqlType 'Nullable _)          = name := Defaultable (Maybe sqlType)

-- * InsertableTable type class/instance
--
-- InsertableTable' 型クラスは InsertRecordFields 型族で"完全な" field list を得る。
-- 各フィールドの型の外側が Defaultable や Maybe のものは実際は INSERT SQL上指定する必要はない。
-- (外側が Maybe ということは NULL 許容かつDEFAULT値が指定されていないので,DEFAUTL は NULL になっているはず)。
--
-- また外側が Defaultable や Maybe であっても
-- (ただ AUTO INCREMENT なカラムでも
-- つまり Mabye st, Defaultable st は st 型であっても問題なく,
-- Defaultable (Maybe st) は Maybe st もしくは st 型であっても問題ないはずである。

-- * Compile functions

-- よしなに
compileInsert
    :: InsertableTable' (Table name cols)
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
    :: InsertableTable' (Table name cols)
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

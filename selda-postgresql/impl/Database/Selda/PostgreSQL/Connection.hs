{-# LANGUAGE GADTs, OverloadedStrings, PatternSynonyms, ViewPatterns, TupleSections #-}
module Database.Selda.PostgreSQL.Connection
    ( module Database.Selda.PostgreSQL.Connection
    , Connection
    ) where

import Database.Selda.Core.Types
import Database.Selda.PostgreSQL.Types
import Database.Selda.PostgreSQL.Encoding
import Database.PostgreSQL.LibPQ hiding (user, pass, db, host)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromJust)
import Control.Monad (void, when, unless)
import Control.Monad.Catch
import Data.ByteString.Lazy (toStrict)
import Data.Text as Text (pack, toLower, take)
import Data.Time (FormatTime, formatTime, defaultTimeLocale)
import Data.UUID.Types (toByteString)
import PostgreSQL.Binary.Encoding as Enc
import PostgreSQL.Binary.Decoding as Dec
import qualified Data.ByteString as BS (ByteString, foldl')
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import Data.Int (Int16, Int32, Int64)

-- | 確立された接続及びメタ情報
-- 同名なので指定は不要のはず
-- type Connection

-- | Prepared Statement Type
type Statement = StmtID

-- | A parameter to a prepared SQL statement.
-- | prepared だけじゃないよね。runStmt でも使っているし
-- | PNul の SqlTypeRep は何の型の NULL かを表わしている
-- TODO: Literal と Parameter と Lit a が何か被っているような...
-- ここでの定義を回避するには Recusive Component が必要か？
-- 一応 Parameter c rep と parameterize すれば selda-core に渡せるが？？
-- data Parameter where
--     PLit :: SqlType' a => a -> Parameter
--     PNul :: SqlTypeRep -> Parameter
--
-- というか型情報月の SqlParam でいいのかな？まあ SqlParam -> SqlTypeRep 関数があればいいけど...
-- type Parameter = (SqlTypeRep, SqlParam)

-- Backend 渡る時点で型は必要ないから SqlParam でいいのか。
-- selda 側は型情報必要なので まだ Lit a と Param = Lit a で。

-- | Execute an SQL statement.
runStmt :: Connection -> Text -> [SqlParam] -> IO (Int, [[SqlValue]])
runStmt c q ps = right <$> pgQueryRunner c False q ps
  where
    right (Right x) = x
    right _         = error "impossible"

-- | Execute an SQL statement and return the last inserted primary key,
--   where the primary key is auto-incrementing.
--   Backends must take special care to make this thread-safe.
runStmtWithPK :: Connection -> Text -> [SqlParam] -> IO Int
runStmtWithPK c q ps = left <$> pgQueryRunner c True q ps
  where
    left (Left x) = x
    left _        = error "impossible"

-- | Prepare a statement using the given statement identifier.
prepareStmt :: Connection -> StmtID -> [SqlTypeRep] -> Text -> IO Statement
prepareStmt = pgPrepare

-- | Execute a prepared statement.
runPrepared :: Connection -> Statement -> [SqlParam] -> IO (Int, [[SqlValue]])
runPrepared = pgRun

-- | Get a list of all columns in the given table, with the type and any
--   modifiers for each column.
--   Return an empty list if the given table does not exist.
getTableInfo :: Connection -> TableName -> IO (TableInfo' SqlTypeRep)
getTableInfo c = pgGetTableInfo c . rawTableName

-- | SQL pretty-printer configuration.
-- これは Database.Selda.Backend.PPConfig が提供するので不要になった.
-- ppConfig :: PPConfig

-- | Close the currently open connection.
-- closeConnection :: SeldaConnection b -> IO ()
-- TODO: PostgreSQL は statement を finalize する必要があるので元々は SeldaConneciton b を受け取っていた。
-- これはどうにしないとね..
closeConnection :: Connection -> [Statement] -> IO ()
closeConnection c _ = finish c

-- | Unique identifier for this backend.
-- TODO: コレ必要なのか？
backendId :: BackendID
backendId = PostgreSQL

-- | Turn on or off foreign key checking, and initiate/commit
--   a transaction.
--
--   When implementing this function, it is safe to assume that
--   @disableForeignKeys True@
--   will always be called exactly once before each
--   @disableForeignKeys False@.
disableForeignKeys :: Connection -> Bool -> IO ()
disableForeignKeys = disableFKs

-- Solution to disable FKs from
-- <https://dba.stackexchange.com/questions/96961/how-to-temporarily-disable-foreign-keys-in-amazon-rds-postgresql>
disableFKs :: Connection -> Bool -> IO ()
disableFKs c True = do
    void $ pgQueryRunner c False "BEGIN TRANSACTION;" []
    void $ pgQueryRunner c False create []
    void $ pgQueryRunner c False dropTbl []
  where
    create = mconcat
      [ "create table if not exists __selda_dropped_fks ("
      , "        seq bigserial primary key,"
      , "        sql text"
      , ");"
      ]
    dropTbl = mconcat
      [ "do $$ declare t record;"
      , "begin"
      , "    for t in select conrelid::regclass::varchar table_name, conname constraint_name,"
      , "            pg_catalog.pg_get_constraintdef(r.oid, true) constraint_definition"
      , "            from pg_catalog.pg_constraint r"
      , "            where r.contype = 'f'"
      , "            and r.connamespace = (select n.oid from pg_namespace n where n.nspname = current_schema())"
      , "        loop"
      , "        insert into __selda_dropped_fks (sql) values ("
      , "            format('alter table if exists %s add constraint %s %s',"
      , "                quote_ident(t.table_name), quote_ident(t.constraint_name), t.constraint_definition));"
      , "        execute format('alter table %s drop constraint %s', quote_ident(t.table_name), quote_ident(t.constraint_name));"
      , "    end loop;"
      , "end $$;"
      ]
disableFKs c False = do
    void $ pgQueryRunner c False restore []
    void $ pgQueryRunner c False "DROP TABLE __selda_dropped_fks;" []
    void $ pgQueryRunner c False "COMMIT;" []
  where
    restore = mconcat
      [ "do $$ declare t record;"
      , "begin"
      , "    for t in select * from __selda_dropped_fks order by seq loop"
      , "        execute t.sql;"
      , "        delete from __selda_dropped_fks where seq = t.seq;"
      , "    end loop;"
      , "end $$;"
      ]

-- 元々は結果をSqlValue sum型に変換していたが,現状は直接 SqlTyp a に
-- ただ以下関数の一部で `SqlString' や `SqlBool' コンストラクで値を取り出そうとしている。
-- PatternSynonim を定義してやる。
-- PatternSynonim だけの場合はパターンへの分解時の関数適用はできないが, ViewPatterns と組合せたら可能
-- (パターンからの構成時は関数適用OK)
-- だから名の通り Pattern に別名を付けるだけ
-- https://mpickering.github.io/papers/pattern-synonyms.pdf
-- https://haskell-explained.gitlab.io/blog/posts/2019/08/27/pattern-synonyms/index.html
-- が一番モチベーション理解できやすいかな。
pattern SqlString str <- (extractStringLike -> Just str)
pattern SqlBool b <- (extractBool -> Just b)

pgGetTableInfo :: Connection -> T.Text -> IO (TableInfo' SqlTypeRep)
pgGetTableInfo c tbl = do
    Right (_, vals) <- pgQueryRunner c False tableinfo []
    if null vals
      then do
        pure $ TableInfo [] [] []
      else do
        Right (_, pkInfo) <- pgQueryRunner c False pkquery []
        Right (_, us) <- pgQueryRunner c False uniquequery []
        let uniques = map splitNames us
        Right (_, fks) <- pgQueryRunner c False fkquery []
        Right (_, ixs) <- pgQueryRunner c False ixquery []
        colInfos <- mapM (describe fks (map toText ixs)) vals
        x <- pure $ TableInfo
          { tableColumnInfos = colInfos
          , tableUniqueGroups = map (map mkColName) uniques
          , tablePrimaryKey = [mkColName pk | [SqlString pk] <- pkInfo]
          }
        pure x
  where
    splitNames = breakNames . toText
    -- TODO: this is super ugly; should really be fixed
    breakNames s =
      case T.break (== '"') s of
        (n, ns) | T.null n  -> []
                | T.null ns -> [n]
                | otherwise -> n : breakNames (T.tail ns)
    toText [SqlString s] = s
    toText _             = error "toText: unreachable"
    tableinfo = mconcat
      [ "SELECT column_name, data_type, is_nullable, column_default LIKE 'nextval(%' "
      , "FROM information_schema.columns "
      , "WHERE table_name = '", tbl, "';"
      ]
    pkquery = mconcat
      [ "SELECT a.attname "
      , "FROM pg_index i "
      , "JOIN pg_attribute a ON a.attrelid = i.indrelid "
      , "  AND a.attnum = ANY(i.indkey) "
      , "WHERE i.indrelid = '\"", tbl, "\"'::regclass "
      , "  AND i.indisprimary;"
      ]
    uniquequery = mconcat
      [ "SELECT string_agg(a.attname, '\"') "
      , "FROM pg_index i "
      , "JOIN pg_attribute a ON a.attrelid = i.indrelid "
      , "  AND a.attnum = ANY(i.indkey) "
      , "WHERE i.indrelid = '\"", tbl, "\"'::regclass "
      , "  AND i.indisunique "
      , "  AND NOT i.indisprimary "
      , "GROUP BY i.indkey;"
      ]
    fkquery = mconcat
      [ "SELECT kcu.column_name, ccu.table_name, ccu.column_name "
      , "FROM information_schema.table_constraints AS tc "
      , "JOIN information_schema.key_column_usage AS kcu "
      , "  ON tc.constraint_name = kcu.constraint_name "
      , "JOIN information_schema.constraint_column_usage AS ccu "
      , "  ON ccu.constraint_name = tc.constraint_name "
      , "WHERE constraint_type = 'FOREIGN KEY' AND tc.table_name='", tbl, "';"
      ]
    ixquery = mconcat
      [ "select a.attname as column_name "
      , "from pg_class t, pg_class i, pg_index ix, pg_attribute a "
      , "where "
      , "t.oid = ix.indrelid "
      , "and i.oid = ix.indexrelid "
      , "and a.attrelid = t.oid "
      , "and a.attnum = ANY(ix.indkey) "
      , "and t.relkind = 'r' "
      , "and not ix.indisunique "
      , "and not ix.indisprimary "
      , "and t.relkind = 'r' "
      , "and t.relname = '", tbl , "';"
      ]
    describe fks ixs [SqlString name, SqlString ty, SqlString nullable, auto] =
      return $ ColumnInfo
        { colName = mkColName name
        , colType = mkTypeRep ty'
        , colIsAutoPrimary = isAuto auto
        , colIsNullable = readBool nullable
        , colHasIndex = name `elem` ixs
        , colFKs =
            [ (mkTableName tblname, mkColName col)
            | [SqlString cname, SqlString tblname, SqlString col] <- fks
            , name == cname
            ]
        }
      where
        ty' = T.toLower ty
        isAuto (SqlBool x) = x
        isAuto _           = False

        readBool :: T.Text -> Bool
        readBool = go . T.toLower
          where
            go "f"     = False
            go "0"     = False
            go "false" = False
            go "n"     = False
            go "no"    = False
            go "off"   = False
            go _       = True

    describe _ _ results =
      throwM $ SqlError $ "bad result from table info query: " ++ show results

pgQueryRunner :: Connection -> Bool -> T.Text -> [SqlParam] -> IO (Either Int (Int, [[SqlValue]]))
pgQueryRunner c return_lastid q ps = do
    mres <- execParams c (encodeUtf8 q') ps Binary
    unlessError c errmsg mres $ \res -> do
      if return_lastid
        then Left <$> getLastId res
        else Right <$> getRows res
  where
    errmsg = "error executing query `" ++ T.unpack q' ++ "'"
    q' | return_lastid = q <> " RETURNING LASTVAL();"
       | otherwise     = q

    getLastId res = (maybe 0 id . fmap readInt) <$> getvalue res 0 0

    readInt :: BS.ByteString -> Int
    readInt = fromIntegral . parse (Dec.int :: Value Int64)

pgRun :: Connection -> StmtID -> [SqlParam] -> IO (Int, [[SqlValue]])
pgRun c sid ps = do
    mres <- execPrepared c (BS.pack $ show sid) (map mkParam ps) Binary
    unlessError c errmsg mres $ getRows
  where
    errmsg = "error executing prepared statement"
    mkParam (Just (_, val, fmt)) = Just (val, fmt)
    mkParam Nothing = Nothing
    -- mkParam (Param p) = case fromSqlValue p of
    --   Just (_, val, fmt) -> Just (val, fmt)
    --   Nothing            -> Nothing

-- | Get all rows from a result.
getRows :: Result -> IO (Int, [[SqlValue]])
getRows res = do
    rows <- ntuples res
    cols <- nfields res
    types <- mapM (ftype res) [0..cols-1]
    affected <- cmdTuples res
    result <- mapM (getRow res types cols) [0..rows-1]
    pure $ case affected of
      Just "" -> (0, result)
      Just s  -> (bsToPositiveInt s, result)
      _       -> (0, result)
  where
    bsToPositiveInt = BS.foldl' (\a x -> a*10+fromIntegral x-48) 0

-- | Get all columns for the given row.
getRow :: Result -> [Oid] -> Column -> Row -> IO [SqlValue]
getRow res types cols row = do
  sequence $ zipWith (getCol res row) [0..cols-1] types

-- | Get the given column.
getCol :: Result -> Row -> Column -> Oid -> IO SqlValue
getCol res row col t = fmap (fmap (t,)) $ getvalue res row col

pgPrepare :: Connection -> StmtID -> [SqlTypeRep] -> T.Text -> IO StmtID
pgPrepare c sid types q = do
    mres <- prepare c (BS.pack $ show sid) (encodeUtf8 q) (Just types')
    unlessError c errmsg mres $ \_ -> pure sid
  where
    types' = map fromSqlType types
    errmsg = "error preparing query `" ++ T.unpack q ++ "'"

-- | Perform the given computation unless an error occurred previously.
unlessError :: Connection -> String -> Maybe Result -> (Result -> IO a) -> IO a
unlessError c msg mres m = do
  case mres of
    Just res -> do
      st <- resultStatus res
      case st of
        BadResponse   -> doError c msg
        FatalError    -> doError c msg
        NonfatalError -> doError c msg
        _             -> m res
    Nothing -> throwM $ DbError "unable to submit query to server"

doError :: Connection -> String -> IO a
doError c msg = do
  me <- errorMessage c
  throwM $ SqlError $ concat
    [ msg
    , maybe "" ((": " ++) . BS.unpack) me
    ]

mkTypeRep :: T.Text ->  Either T.Text SqlTypeRep
mkTypeRep "bigserial"                = Right TRowID
mkTypeRep "int8"                     = Right TInt
mkTypeRep "bigint"                   = Right TInt
mkTypeRep "float8"                   = Right TDouble
mkTypeRep "double precision"         = Right TDouble
-- mkTypeRep "timestamp with time zone" = Right TDateTime
mkTypeRep "bytea"                    = Right TBlob
mkTypeRep "text"                     = Right TText
mkTypeRep "boolean"                  = Right TBool
-- mkTypeRep "date"                     = Right TDate
-- mkTypeRep "time with time zone"      = Right TTime
mkTypeRep "uuid"                     = Right TUUID
-- mkTypeRep "jsonb"                    = Right TJSON
mkTypeRep typ                        = Left typ

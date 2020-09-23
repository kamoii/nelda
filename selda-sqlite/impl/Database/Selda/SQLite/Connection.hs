{-# LANGUAGE GADTs, OverloadedStrings #-}
module Database.Selda.SQLite.Connection where

import Database.Selda.Core.Types
import Database.Selda.SQLite.Types
import Database.Selda.SQLite.Parser2
import Data.Text (Text)
import Data.Maybe (fromJust)
import Control.Monad (void, when, unless)
import Control.Monad.Catch
import Data.ByteString.Lazy (toStrict)
import Data.Dynamic
import Data.Text as Text (pack, toLower, take)
import Data.Time (FormatTime, formatTime, defaultTimeLocale)
import Data.UUID.Types (toByteString)
import Database.SQLite3
import System.Directory (makeAbsolute)

-- | 確立された接続及びメタ情報
type Connection = Database

-- | Prepared Statement Type
type PreparedStatement = Statement

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
runStmt db q ps = snd <$> sqliteQueryRunner db q ps

-- | Execute an SQL statement and return the last inserted primary key,
--   where the primary key is auto-incrementing.
--   Backends must take special care to make this thread-safe.
runStmtWithPK :: Connection -> Text -> [SqlParam] -> IO Int
runStmtWithPK db q ps = fst <$> sqliteQueryRunner db q ps

-- | Prepare a statement using the given statement identifier.
prepareStmt :: Connection -> StmtID -> [SqlTypeRep] -> Text -> IO PreparedStatement
prepareStmt db _ _ = sqlitePrepare db

-- | Execute a prepared statement.
runPrepared :: Connection -> PreparedStatement -> [SqlParam] -> IO (Int, [[SqlValue]])
runPrepared = sqliteRunPrepared

-- | Get a list of all columns in the given table, with the type and any
--   modifiers for each column.
--   Return an empty list if the given table does not exist.
getTableInfo :: Connection -> TableName -> IO (TableInfo' SqlTypeRep)
getTableInfo db = sqliteGetTableInfo db . fromTableName

-- | SQL pretty-printer configuration.
-- これは Database.Selda.Backend.PPConfig が提供するので不要になった.
-- ppConfig :: PPConfig

-- | Close the currently open connection.
-- closeConnection :: SeldaConnection b -> IO ()
-- TODO: SQLite は statement を finalize する必要があるので元々は SeldaConneciton b を受け取っていた。
-- これはどうにしないとね..
closeConnection :: Connection -> [PreparedStatement] -> IO ()
closeConnection db stmts = do
    mapM_ finalize stmts
    close db

-- | Unique identifier for this backend.
-- TODO: コレ必要なのか？
backendId :: BackendID
backendId = SQLite

-- | Turn on or off foreign key checking, and initiate/commit
--   a transaction.
--
--   When implementing this function, it is safe to assume that
--   @disableForeignKeys True@
--   will always be called exactly once before each
--   @disableForeignKeys False@.
disableForeignKeys :: Connection -> Bool -> IO ()
disableForeignKeys = disableFKs

-- * 内部関数

type QueryRunner a = Text -> [SQLData] -> IO a

sqliteQueryRunner :: Database -> QueryRunner (Int, (Int, [[SQLData]]))
sqliteQueryRunner db qry params = do
    eres <- try $ do
      stm <- prepare db qry
      sqliteRunStmt db stm params `finally` do
        finalize stm
    case eres of
      Left e@(SQLError{}) -> throwM (SqlError (show e))
      Right res           -> pure res

sqliteRunStmt :: Database -> Statement -> [SQLData] -> IO (Int, (Int, [[SQLData]]))
sqliteRunStmt db stm params = do
  bind stm params
  rows <- getRows stm []
  rid <- lastInsertRowId db
  cs <- changes db
  return (fromIntegral rid, (cs, rows))

getRows :: Statement -> [[SQLData]] -> IO [[SQLData]]
getRows s acc = do
  res <- step s
  case res of
    Row -> do
      cs <- columns s
      getRows s (cs : acc)
    _ -> do
      return $ reverse acc

sqlitePrepare :: Database -> Text -> IO Statement
sqlitePrepare db qry = do
  eres <- try $ prepare db qry
  case eres of
    Left e@(SQLError{}) -> throwM (SqlError (show e))
    Right r             -> pure r

sqliteRunPrepared :: Database -> Statement -> [SQLData] -> IO (Int, [[SQLData]])
sqliteRunPrepared db stm params = do
  eres <- try $ do
    sqliteRunStmt db stm params `finally` do
      clearBindings stm
      reset stm
  case eres of
    Left e@(SQLError{}) -> throwM (SqlError (show e))
    Right res           -> return (snd res)

sqliteGetTableInfo :: Database -> Text -> IO (TableInfo' SqlTypeRep)
sqliteGetTableInfo db tbl = do
    cols <- (snd . snd) <$> sqliteQueryRunner db tblinfo []
    fks <- (snd . snd) <$> sqliteQueryRunner db fklist []
    createQuery <- (snd . snd) <$> sqliteQueryRunner db autos []
    let cs = case createQuery of
          [[SQLText q]] -> colsFromQuery q
          _               -> []
    ixs <- mapM indexInfo . snd . snd =<< sqliteQueryRunner db indexes []
    colInfos <- mapM (describe fks ixs cs) cols
    return $ TableInfo
      { tableColumnInfos = colInfos
      , tableUniqueGroups =
        [ map mkColName names
        | (names, "u") <- ixs
        ]
      , tablePrimaryKey = concat
        [ concat
          [ map mkColName names
          | (names, "pk") <- ixs
          ]
        , [ colName ci
          | ci <- colInfos
          , colIsAutoPrimary ci
          ]
        ]
      }
  where
    tblinfo = mconcat ["PRAGMA table_info(", tbl, ");"]
    indexes = mconcat ["PRAGMA index_list(", tbl, ");"]
    fklist = mconcat ["PRAGMA foreign_key_list(", tbl, ");"]
    autos = mconcat ["SELECT sql FROM sqlite_master WHERE name = ", tbl, ";"]
    ixinfo name = mconcat ["PRAGMA index_info(", name, ");"]

    -- TODO: SQLite の別名の扱いについて再度検討の余地がありか
    -- 正式なものに近いのかな？
    toTypeRep _ "text"                      = Right TText
    toTypeRep _ "double"                    = Right TFloat
    toTypeRep _ "boolean"                   = Right TBoolean
    -- toTypeRep _ "datetime"                  = Right TDateTime
    -- toTypeRep _ "date"                      = Right TDate
    -- toTypeRep _ "time"                      = Right TTime
    toTypeRep _ "blob"                      = Right TBlob
    toTypeRep pk s | Text.take 3 s == "int"  = Right $ if pk then TRowID else TInteger
    toTypeRep _ typ                         = Left typ

    indexInfo [_, SQLText ixname, _, SQLText itype, _] = do
      let q = ixinfo ixname
      info <- (snd . snd) <$> sqliteQueryRunner db q []
      return $ (map (\[_,_,SQLText name] -> name) info, itype)
    indexInfo _ = do
      error "unreachable"

    describe fks ixs cs [_, SQLText name, SQLText ty, SQLInteger nonnull, _, SQLInteger pk] = do
      let ty' = Text.toLower ty
      return $ ColumnInfo
        { colName = mkColName name
        , colType = toTypeRep (pk == 1) ty'
        , colIsAutoPrimary = snd $ fromJust $ lookup name cs
        , colHasIndex = any (== ([name], "c")) ixs
        , colIsNullable = nonnull == 0
        , colFKs =
            [ (mkTableName reftbl, mkColName refkey)
            | (_:_:SQLText reftbl:SQLText key:SQLText refkey:_) <- fks
            , key == name
            ]
        }
    describe _ _ _ result = do
      throwM $ SqlError $ "bad result from PRAGMA table_info: " ++ show result

disableFKs :: Database -> Bool -> IO ()
disableFKs db disable = do
    unless disable $ void $ sqliteQueryRunner db "COMMIT;" []
    void $ sqliteQueryRunner db q []
    when disable $ void $ sqliteQueryRunner db "BEGIN TRANSACTION;" []
  where
    q | disable   = "PRAGMA foreign_keys = OFF;"
      | otherwise = "PRAGMA foreign_keys = ON;"

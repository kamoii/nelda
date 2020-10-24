{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Nelda.Compile.SQL where

import Database.Nelda.SQL.Types
import Database.Nelda.Types (Sql(Sql))
import Database.Nelda.Schema (TableName)

import Control.Monad.State (liftM2, put, get, runState, State)
import Data.Text (replace, Text)
import qualified Data.Text as Text
import Data.List (sort, group)
import Database.Nelda.Compile.Quoting (quoteTableName)

-- | Compile an SQL AST into a parameterized SQL query.
compileSQL :: SQL -> (Sql, [Param])
compileSQL = (\(sql, ps) -> (Sql sql, ps)) . runPP . ppSql

-- | O(n log n) equivalent of @nub . sort@
snub :: (Ord a, Eq a) => [a] -> [a]
snub = map head . group . sort

-- | SQL pretty-printer. The state is the list of SQL parameters to the
--   prepared statement.
type PP = State PPState

data PPState = PPState
    { ppParams  :: ![Param]
    , ppParamNS :: !Int
    , ppQueryNS :: !Int
    }

-- | Run a pretty-printer.
runPP :: PP Text -> (Text, [Param])
runPP pp =
    case runState pp (PPState [] 1 0) of
    (q, st) -> (q, reverse (ppParams st))

-- | Compile a single column expression.
compExp :: Exp a -> (Text, [Param])
compExp = runPP . ppCol

-- | Compile a raw SQL fragment.
compRaw :: QueryFragment -> (Text, [Param])
compRaw = runPP . ppRaw

-- | Compile an @UPATE@ statement.
compUpdate
    :: TableName
    -> Exp Bool
    -> [(ColName, SomeCol)]
    -> (Text, [Param])
compUpdate tbl p cs = runPP ppUpd
  where
    ppUpd = do
      updates <- mapM ppUpdate cs
      check <- ppCol p
      pure $ Text.unwords
        [ "UPDATE", quoteTableName tbl
        , "SET", set updates
        , "WHERE", check
        ]
    ppUpdate (n, c) = do
      let n' = fromColName n
      c' <- ppSomeCol c
      let upd = Text.unwords [n', "=", c']
      if n' == c'
        then pure $ Left upd
        else pure $ Right upd
    -- if the update doesn't change anything, pick an arbitrary column to
    -- set to itself just to satisfy SQL's syntactic rules
    set us =
      case [u | Right u <- us] of
        []  -> set (take 1 [Right u | Left u <- us])
        us' -> Text.intercalate ", " us'

-- | Convert a column name into a string, with quotes.
fromColName :: ColName -> Text
fromColName (ColName cn) = mconcat ["\"", escapeQuotes cn, "\""]
  where
    escapeQuotes = Data.Text.replace "\"" "\"\""

-- | Compile a @DELETE@ statement.
compDelete :: TableName -> Exp Bool -> (Text, [Param])
compDelete tbl p = runPP ppDelete
  where
    ppDelete = do
      c' <- ppCol p
      pure $ Text.unwords ["DELETE FROM", quoteTableName tbl, "WHERE", c']

-- | Pretty-print a literal as a named parameter and save the
--   name-value binding in the environment.
ppLit :: Lit a -> PP Text
ppLit LNull     = pure "NULL"
ppLit (LJust l) = ppLit l
ppLit l         = do
    PPState ps ns qns <- get
    put $ PPState (Param l : ps) (succ ns) qns
    return $ Text.pack ('$':show ns)

-- | Generate a unique name for a subquery.
freshQueryName :: PP Text
freshQueryName = do
  PPState ps ns qns <- get
  put $ PPState ps ns (succ qns)
  return $ Text.pack ('q':show qns)

-- | Pretty-print an SQL AST.
ppSql :: SQL -> PP Text
ppSql (SQL cs src r gs ord lim _live dist) = do
  cs' <- mapM ppSomeCol cs
  src' <- ppSrc src
  r' <- ppRestricts r
  gs' <- ppGroups gs
  ord' <- ppOrder ord
  lim' <- ppLimit lim
  pure $ mconcat
    [ "SELECT ", if dist then "DISTINCT " else "", result cs'
    , src'
    , r'
    , gs'
    , ord'
    , lim'
    ]
  where
    result []  = "1"
    result cs' = Text.intercalate ", " cs'

    ppSrc EmptyTable = do
      qn <- freshQueryName
      pure $ " FROM (SELECT NULL LIMIT 0) AS " <> qn
    ppSrc (RawSql raw) = do
      s <- ppRaw raw
      qn <- freshQueryName
      pure (" FROM (" <> s <> ") AS " <> qn)
    ppSrc (TableName n)  = do
      pure $ " FROM " <> quoteTableName n
    ppSrc (Product [])   = do
      pure ""
    ppSrc (Product sqls) = do
      srcs <- mapM ppSql (reverse sqls)
      qs <- flip mapM ["(" <> s <> ")" | s <- srcs] $ \q -> do
        qn <- freshQueryName
        pure (q <> " AS " <> qn)
      pure $ " FROM " <> Text.intercalate ", " qs
    ppSrc (Values row rows) = do
      row' <- Text.intercalate ", " <$> mapM ppSomeCol row
      rows' <- mapM ppRow rows
      qn <- freshQueryName
      pure $ mconcat
        [ " FROM (SELECT "
        , Text.intercalate " UNION ALL SELECT " (row':rows')
        , ") AS "
        , qn
        ]
    ppSrc (Join jointype on left right) = do
      l' <- ppSql left
      r' <- ppSql right
      on' <- ppCol on
      lqn <- freshQueryName
      rqn <- freshQueryName
      pure $ mconcat
        [ " FROM (", l', ") AS ", lqn
        , " ",  ppJoinType jointype, " (", r', ") AS ", rqn
        , " ON ", on'
        ]
    ppSrc (Union union_all left right) = do
      qs <- mapM ppSql [left, right]
      qn <- freshQueryName
      let union = if union_all then " UNION ALL " else " UNION "
      pure $ mconcat
        [ " FROM ("
        , Text.intercalate union qs
        , ") AS "
        , qn
        ]

    ppJoinType LeftJoin  = "LEFT JOIN"
    ppJoinType InnerJoin = "JOIN"

    ppRow xs = do
      ls <- sequence [ppLit l | Param l <- xs]
      pure $ Text.intercalate ", " ls

    ppRestricts [] = pure ""
    ppRestricts rs = ppCols rs >>= \rs' -> pure $ " WHERE " <> rs'

    ppGroups [] = pure ""
    ppGroups grps = do
      cls <- sequence [ppCol c | Some c <- grps]
      pure $ " GROUP BY " <> Text.intercalate ", " cls

    ppOrder [] = pure ""
    ppOrder os = do
      os' <- sequence [(<> (" " <> ppOrd o)) <$> ppCol c | (o, Some c) <- os]
      pure $ " ORDER BY " <> Text.intercalate ", " os'

    ppOrd Asc = "ASC"
    ppOrd Desc = "DESC"

    ppLimit Nothing =
      pure ""
    ppLimit (Just (off, limit)) =
      pure $ " LIMIT " <> ppInt limit <> " OFFSET " <> ppInt off

    ppInt = Text.pack . show

ppRaw :: QueryFragment -> PP Text
ppRaw (RawText t)  = pure t
ppRaw (RawExp e)   = ppCol e
ppRaw (RawCat a b) = liftM2 (<>) (ppRaw a) (ppRaw b)

ppSomeCol :: SomeCol -> PP Text
ppSomeCol (Some c)    = ppCol c
ppSomeCol (Named n c) = do
  c' <- ppCol c
  pure $ c' <> " AS " <> fromColName n

ppCols :: [Exp Bool] -> PP Text
ppCols cs = do
  cs' <- mapM ppCol (reverse cs)
  pure $ "(" <> Text.intercalate ") AND (" cs' <> ")"

-- Cast に使われているが, SqlType instance が全て Castable ではない。
-- DB毎に変わる。一旦 Cast は無効化する。
--
-- ppType :: SqlTypeRep -> PP Text
-- ppType t = do
--   c <- ppConfig <$> get
--   pure $ Cfg.ppType c t

-- 使ってないので...
-- ppTypePK :: SqlTypeRep -> PP Text
-- ppTypePK t = do
--   c <- ppConfig <$> get
--   pure $ Cfg.ppTypePK c t

ppCol :: Exp a -> PP Text
ppCol (Col name)     = pure (fromColName name)
ppCol (Lit l)        = ppLit l
ppCol (BinOp op a b) = ppBinOp op a b
ppCol (UnOp op a)    = ppUnOp op a
ppCol (NulOp a)      = ppNulOp a
ppCol (Raw e)        = pure e
ppCol (Fun2 f a b)   = do
  a' <- ppCol a
  b' <- ppCol b
  pure $ mconcat [f, "(", a', ", ", b', ")"]
ppCol (If a b c)     = do
  a' <- ppCol a
  b' <- ppCol b
  c' <- ppCol c
  pure $ mconcat ["CASE WHEN ", a', " THEN ", b', " ELSE ", c', " END"]
ppCol (AggrEx f x)   = ppUnOp (Fun f) x
-- ppCol (Cast t x)     = do
--   x' <- ppCol x
--   t' <- ppType t
--   pure $ mconcat ["CAST(", x', " AS ", t', ")"]
ppCol (Cast _t _x)    = error "CAST not supported now"
ppCol (InList x xs) = do
  x' <- ppCol x
  xs' <- mapM ppCol xs
  pure $ mconcat [x', " IN (", Text.intercalate ", " xs', ")"]
ppCol (InQuery x q) = do
  x' <- ppCol x
  q' <- ppSql q
  pure $ mconcat [x', " IN (", q', ")"]

ppNulOp :: NulOp a -> PP Text
ppNulOp (Fun0 f) = pure $ f <> "()"

ppUnOp :: UnOp a b -> Exp a -> PP Text
ppUnOp op c = do
  c' <- ppCol c
  pure $ case op of
    Abs    -> "ABS(" <> c' <> ")"
    Sgn    -> "SIGN(" <> c' <> ")"
    Neg    -> "-(" <> c' <> ")"
    Not    -> "NOT(" <> c' <> ")"
    IsNull -> "(" <> c' <> ") IS NULL"
    Fun f  -> f <> "(" <> c' <> ")"

ppBinOp :: BinOp a b c -> Exp a -> Exp b -> PP Text
ppBinOp op a b = do
    a' <- ppCol a
    b' <- ppCol b
    pure $ paren a a' <> " " <> ppOp op <> " " <> paren b b'
  where
    paren :: Exp a -> Text -> Text
    paren (Col{}) c = c
    paren (Lit{}) c = c
    paren _ c       = "(" <> c <> ")"

    ppOp :: BinOp a b c -> Text
    ppOp Gt   = ">"
    ppOp Lt   = "<"
    ppOp Gte  = ">="
    ppOp Lte  = "<="
    ppOp Eq   = "="
    ppOp Neq  = "!="
    ppOp And  = "AND"
    ppOp Or   = "OR"
    ppOp Add  = "+"
    ppOp Sub  = "-"
    ppOp Mul  = "*"
    ppOp Div  = "/"
    ppOp Like = "LIKE"
    ppOp (CustomOp s) = s

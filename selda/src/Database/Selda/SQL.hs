{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables, RecordWildCards, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE RankNTypes, CPP, MultiParamTypeClasses, TypeApplications #-}
-- | SQL AST and parameters for prepared statements.
module Database.Selda.SQL where
import Data.String
import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Database.Selda.SqlType
import Database.Selda.Types
import Database.Selda.Backend.Types as BE
#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup (Semigroup (..))
#endif

data QueryFragment where
  RawText :: !Text -> QueryFragment
  RawExp  :: !(Exp a) -> QueryFragment
  RawCat  :: !QueryFragment -> !QueryFragment -> QueryFragment

deriving instance Show QueryFragment

instance Semigroup QueryFragment where
  (<>) = RawCat

instance IsString QueryFragment where
  fromString = RawText . fromString

-- | A source for an SQL query.
data SqlSource
 = TableName !TableName
 | Product ![SQL]
 | Union !Bool !SQL !SQL
 | Join !JoinType !(Exp Bool) !SQL !SQL
 | Values ![SomeCol] ![[Param]]
 | RawSql !QueryFragment
 | EmptyTable

deriving instance Show SqlSource

-- | Type of join to perform.
data JoinType = InnerJoin | LeftJoin

deriving instance Show JoinType

-- | AST for SQL queries.
data SQL = SQL
  { cols       :: ![SomeCol]
  , source     :: !SqlSource
  , restricts  :: ![Exp Bool]
  , groups     :: ![SomeCol]
  , ordering   :: ![(Order, SomeCol)]
  , limits     :: !(Maybe (Int, Int))
  , liveExtras :: ![SomeCol] -- ^ Columns which are never considered dead.
  , distinct   :: !Bool
  }

deriving instance Show SQL

instance Names QueryFragment where
  allNamesIn (RawText _)  = []
  allNamesIn (RawExp e)   = allNamesIn e
  allNamesIn (RawCat a b) = allNamesIn a ++ allNamesIn b

instance Names SqlSource where
  allNamesIn (Product qs)   = concatMap allNamesIn qs
  allNamesIn (Join _ e l r) = allNamesIn e ++ concatMap allNamesIn [l, r]
  allNamesIn (Values vs _)  = allNamesIn vs
  allNamesIn (TableName _)  = []
  allNamesIn (RawSql r)     = allNamesIn r
  allNamesIn (EmptyTable)   = []
  allNamesIn (Union _ l r)  = concatMap allNamesIn [l, r]

instance Names SQL where
  -- Note that we don't include @cols@ here: the names in @cols@ are not
  -- necessarily used, only declared.
  allNamesIn (SQL{..}) = concat
    [ allNamesIn groups
    , concatMap (allNamesIn . snd) ordering
    , allNamesIn restricts
    , allNamesIn source
    ]

-- | Build a plain SQL query with the given columns and source, with no filters,
--   ordering, etc.
sqlFrom :: [SomeCol] -> SqlSource -> SQL
sqlFrom cs src = SQL
  { cols = cs
  , source = src
  , restricts = []
  , groups = []
  , ordering = []
  , limits = Nothing
  , liveExtras = []
  , distinct = False
  }

-- | The order in which to sort result rows.
data Order = Asc | Desc
  deriving (Show, Ord, Eq)

-- | A parameter to a prepared SQL statement.
data Param where
   Param :: !(Lit a) -> Param

deriving instance Show Param

-- TODO: 復活は必要か？
-- instance Show Param where
--   show (Param l) = "Param " <> show l

-- | Create a parameter from the given value.
param :: SqlType a => a -> Param
param = Param . mkLit

-- | The SQL type of the given parameter.
paramType :: Param -> SqlTypeRep
paramType (Param p) = litType p

paramToSqlParam :: Param -> BE.SqlParam
paramToSqlParam (Param l) = litToSqlParam l

-- | A type-erased column, which may also be renamed.
--   Only for internal use.
data SomeCol where
  Some  :: !(Exp a) -> SomeCol
  Named :: !ColName -> !(Exp a) -> SomeCol

deriving instance Show SomeCol

data UntypedCol where
  Untyped :: !(Exp a) -> UntypedCol

deriving instance Show UntypedCol

-- | Turn a renamed column back into a regular one.
--   If the column was renamed, it will be represented by a literal column,
--   and not its original expression.
hideRenaming :: SomeCol -> UntypedCol
hideRenaming (Named n _) = Untyped (Col n)
hideRenaming (Some c)    = Untyped c

-- | Underlying column expression type, parameterised over the type of
--   SQL queries.
data Exp a where
  Col     :: !ColName -> Exp a
  Lit     :: !(Lit a) -> Exp a
  BinOp   :: !(BinOp a b c) -> !(Exp a) -> !(Exp b) -> Exp c
  UnOp    :: !(UnOp a b) -> !(Exp a) -> Exp b
  NulOp   :: !(NulOp a) -> Exp a
  Fun2    :: !Text -> !(Exp a) -> !(Exp b) -> Exp c
  If      :: !(Exp Bool) -> !(Exp a) -> !(Exp a) -> Exp a
  Cast    :: !SqlTypeRep -> !(Exp a) -> Exp b
  Raw     :: !Text -> Exp a
  AggrEx  :: !Text -> !(Exp a) -> Exp b
  InList  :: !(Exp a) -> ![Exp a] -> Exp Bool
  InQuery :: !(Exp a) -> !SQL -> Exp Bool

deriving instance Show (Exp a)

data NulOp a where
  Fun0 :: !Text -> NulOp a

deriving instance Show (NulOp a)

data UnOp a b where
  Abs    :: UnOp a a
  Not    :: UnOp Bool Bool
  Neg    :: UnOp a a
  Sgn    :: UnOp a a
  IsNull :: UnOp (Maybe a) Bool
  Fun    :: !Text -> UnOp a b

deriving instance Show (UnOp a b)

data BinOp a b c where
  Gt   :: BinOp a a Bool
  Lt   :: BinOp a a Bool
  Gte  :: BinOp a a Bool
  Lte  :: BinOp a a Bool
  Eq   :: BinOp a a Bool
  Neq  :: BinOp a a Bool
  And  :: BinOp Bool Bool Bool
  Or   :: BinOp Bool Bool Bool
  Add  :: BinOp a a a
  Sub  :: BinOp a a a
  Mul  :: BinOp a a a
  Div  :: BinOp a a a
  Like :: BinOp Text Text Bool
  CustomOp :: !Text -> BinOp a b c

deriving instance Show (BinOp a b c)

-- | Any type which may contain column names.
class Names a where
  -- | Get all column names used in the given expression.
  allNamesIn :: a -> [ColName]

instance Names a => Names [a] where
  allNamesIn = concatMap allNamesIn

instance Names (Exp a) where
  allNamesIn (Col n)       = [n]
  allNamesIn (Lit _)       = []
  allNamesIn (BinOp _ a b) = allNamesIn a ++ allNamesIn b
  allNamesIn (UnOp _ a)    = allNamesIn a
  allNamesIn (NulOp _)     = []
  allNamesIn (Fun2 _ a b)  = allNamesIn a ++ allNamesIn b
  allNamesIn (If a b c)    = allNamesIn a ++ allNamesIn b ++ allNamesIn c
  allNamesIn (Cast _ x)    = allNamesIn x
  allNamesIn (AggrEx _ x)  = allNamesIn x
  allNamesIn (InList x xs) = concatMap allNamesIn (x:xs)
  allNamesIn (InQuery x q) = allNamesIn x ++ allNamesIn q
  allNamesIn (Raw _)       = []

instance Names SomeCol where
  allNamesIn (Some c)    = allNamesIn c
  allNamesIn (Named n c) = n : allNamesIn c

instance Names UntypedCol where
  allNamesIn (Untyped c) = allNamesIn c

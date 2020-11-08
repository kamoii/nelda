{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Database.Nelda.SQL.Types where

import Data.String (IsString (..))
import Data.Text (Text, append, pack)
import Database.Nelda.Backend.Types (SqlParam, nullSqlParam)
import Database.Nelda.Schema (TableName)
import Database.Nelda.Schema.Column.SqlColumnTypeRepAndKind (SqlColumnTypeRep)
import Database.Nelda.SqlType (SqlType, toSqlParam)

-- * QueryFragment

data QueryFragment where
    RawText :: !Text -> QueryFragment
    RawExp :: !(Exp a) -> QueryFragment
    RawCat :: !QueryFragment -> !QueryFragment -> QueryFragment

deriving instance Show QueryFragment

instance Semigroup QueryFragment where
    (<>) = RawCat

instance IsString QueryFragment where
    fromString = RawText . fromString

-- * SqlSource

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

-- * JoinType

-- | Type of join to perform.
data JoinType = InnerJoin | LeftJoin

deriving instance Show JoinType

-- * SQL(*)

-- | AST for SQL queries.
data SQL = SQL
    { cols :: ![SomeCol]
    , source :: !SqlSource
    , restricts :: ![Exp Bool]
    , groups :: ![SomeCol]
    , ordering :: ![(Order, SomeCol)]
    , limits :: !(Maybe (Int, Int))
    , -- | Columns which are never considered dead.
      liveExtras :: ![SomeCol]
    , distinct :: !Bool
    }

deriving instance Show SQL

-- | Build a plain SQL query with the given columns and source, with no filters,
--   ordering, etc.
sqlFrom :: [SomeCol] -> SqlSource -> SQL
sqlFrom cs src =
    SQL
        { cols = cs
        , source = src
        , restricts = []
        , groups = []
        , ordering = []
        , limits = Nothing
        , liveExtras = []
        , distinct = False
        }

-- * Order

-- | The order in which to sort result rows.
data Order = Asc | Desc
    deriving (Show, Ord, Eq)

-- * Lit(*)

-- TODO: LNull/LJust 必要か？
-- 結局 Nothing 使うから...
-- LNull/LJust は恐らく過去の遺物。
-- selda のコードベースでも意味ある使い方をしてあることは少ない。
-- LNull だけは若干あるかな...
-- LLiteral の Nothing は parameter として渡される(SQL中に NULL キーワードは使われない)
--
-- selda だと SqlType (Maybe a) instance で LJust と LNull が使われている。
-- https://github.com/valderman/selda/blob/cd64be78c00761b0ea2969da1739fff3f8cdc8c0/selda/src/Database/Selda/SqlType.hs#L392
-- なので Maybe a の値が NULL の際は parameter ではなく,SQL中の NULL キーワードで表現される。
--
-- Nelda だと LLiteral の Nothing として表現されて NULL が parameter で渡されるのは微妙か...
-- 改善はできると思うが。isNullSqlParam

data Lit a where
    LLiteral :: SqlType a => a -> Lit a
    LNull :: SqlType a => Lit a

-- LJust    :: SqlType a => !(Lit a) -> Lit (Maybe a)

instance Show (Lit a) where
    show (LLiteral a) = show a
    show (LNull) = "Nothing"

mkLit :: SqlType a => a -> Lit a
mkLit = LLiteral

mkNullLit :: SqlType a => Lit a
mkNullLit = LNull

litToSqlParam :: Lit a -> SqlParam
litToSqlParam (LLiteral a) = toSqlParam a
litToSqlParam LNull = nullSqlParam
-- * Param(*)

-- SQL Type

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

mkParam :: SqlType a => Lit a -> Param
mkParam = Param

paramToSqlParam :: Param -> SqlParam
paramToSqlParam (Param l) = litToSqlParam l
-- * ColName

-- | This a name reference. It chould be column name but also it chould be alias name.
newtype ColName = ColName Text
    deriving (Eq, Ord, Show)

instance IsString ColName where
    fromString = ColName . pack

-- | Create a column name.
mkColName :: Text -> ColName
mkColName = ColName

-- | Modify the given column name using the given function.
modColName :: ColName -> (Text -> Text) -> ColName
modColName (ColName cn) f = ColName (f cn)

-- | Add a prefix to a column name.
addColPrefix :: ColName -> Text -> ColName
addColPrefix (ColName cn) s = ColName $ Data.Text.append s cn

-- | Add a suffix to a column name.
addColSuffix :: ColName -> Text -> ColName
addColSuffix (ColName cn) s = ColName $ Data.Text.append cn s

-- * SomeCol/UntypedCol(*)

-- | A type-erased column, which may also be renamed.
--   Only for internal use.
data SomeCol where
    Some :: !(Exp a) -> SomeCol
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
hideRenaming (Some c) = Untyped c

-- * Exp

-- | Underlying column expression type, parameterised over the type of
--   SQL queries.
data Exp a where
    Col :: !ColName -> Exp a
    Lit :: !(Lit a) -> Exp a
    BinOp :: !(BinOp a b c) -> !(Exp a) -> !(Exp b) -> Exp c
    UnOp :: !(UnOp a b) -> !(Exp a) -> Exp b
    NulOp :: !(NulOp a) -> Exp a
    Fun2 :: !Text -> !(Exp a) -> !(Exp b) -> Exp c
    If :: !(Exp Bool) -> !(Exp a) -> !(Exp a) -> Exp a
    Cast :: !SqlColumnTypeRep -> !(Exp a) -> Exp b
    Raw :: !Text -> Exp a
    AggrEx :: !Text -> !(Exp a) -> Exp b
    InList :: !(Exp a) -> ![Exp a] -> Exp Bool
    InQuery :: !(Exp a) -> !SQL -> Exp Bool

deriving instance Show (Exp a)

-- * NullOp/UnOP/BinOp

data NulOp a where
    Fun0 :: !Text -> NulOp a

deriving instance Show (NulOp a)

data UnOp a b where
    Abs :: UnOp a a
    Not :: UnOp Bool Bool
    Neg :: UnOp a a
    Sgn :: UnOp a a
    IsNull :: UnOp a Bool
    Fun :: !Text -> UnOp a b

deriving instance Show (UnOp a b)

data BinOp a b c where
    Gt :: BinOp a a Bool
    Lt :: BinOp a a Bool
    Gte :: BinOp a a Bool
    Lte :: BinOp a a Bool
    Eq :: BinOp a a Bool
    Neq :: BinOp a a Bool
    And :: BinOp Bool Bool Bool
    Or :: BinOp Bool Bool Bool
    Add :: BinOp a a a
    Sub :: BinOp a a a
    Mul :: BinOp a a a
    Div :: BinOp a a a
    IntDiv ::
        -- | 整数除算
        BinOp a a a
    Like :: BinOp Text Text Bool
    CustomOp :: !Text -> BinOp a b c

deriving instance Show (BinOp a b c)

-- * Names type class/instances

-- | Any type which may contain column names.
class Names a where
    -- | Get all column names used in the given expression.
    allNamesIn :: a -> [ColName]

instance Names a => Names [a] where
    allNamesIn = concatMap allNamesIn

instance Names (Exp a) where
    allNamesIn (Col n) = [n]
    allNamesIn (Lit _) = []
    allNamesIn (BinOp _ a b) = allNamesIn a ++ allNamesIn b
    allNamesIn (UnOp _ a) = allNamesIn a
    allNamesIn (NulOp _) = []
    allNamesIn (Fun2 _ a b) = allNamesIn a ++ allNamesIn b
    allNamesIn (If a b c) = allNamesIn a ++ allNamesIn b ++ allNamesIn c
    allNamesIn (Cast _ x) = allNamesIn x
    allNamesIn (AggrEx _ x) = allNamesIn x
    allNamesIn (InList x xs) = concatMap allNamesIn (x : xs)
    allNamesIn (InQuery x q) = allNamesIn x ++ allNamesIn q
    allNamesIn (Raw _) = []

instance Names SomeCol where
    allNamesIn (Some c) = allNamesIn c
    allNamesIn (Named n c) = n : allNamesIn c

instance Names UntypedCol where
    allNamesIn (Untyped c) = allNamesIn c

instance Names QueryFragment where
    allNamesIn (RawText _) = []
    allNamesIn (RawExp e) = allNamesIn e
    allNamesIn (RawCat a b) = allNamesIn a ++ allNamesIn b

instance Names SqlSource where
    allNamesIn (Product qs) = concatMap allNamesIn qs
    allNamesIn (Join _ e l r) = allNamesIn e ++ concatMap allNamesIn [l, r]
    allNamesIn (Values vs _) = allNamesIn vs
    allNamesIn (TableName _) = []
    allNamesIn (RawSql r) = allNamesIn r
    allNamesIn (EmptyTable) = []
    allNamesIn (Union _ l r) = concatMap allNamesIn [l, r]

instance Names SQL where
    -- Note that we don't include @cols@ here: the names in @cols@ are not
    -- necessarily used, only declared.
    allNamesIn (SQL{..}) =
        concat
            [ allNamesIn groups
            , concatMap (allNamesIn . snd) ordering
            , allNamesIn restricts
            , allNamesIn source
            ]

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Nelda.Query.Monad where

import Database.Nelda.SQL.Types (mkColName, addColSuffix, SomeCol(Named), ColName, Exp(Col), UntypedCol(Untyped), UntypedCol, SomeCol, Exp, SQL)
import Database.Nelda.Query.Name
import Control.Monad.State (put, get, runState, State)
import Data.Text (pack)

-- | An SQL query.
newtype Query s a = Query {unQ :: State GenState a}
    deriving (Functor, Applicative, Monad)

-- | Run a query computation from an initial state.
runQueryM :: Scope -> Query s a -> (a, GenState)
runQueryM scope = flip runState (initState scope) . unQ

-- | Run a query computation in isolation, but reusing the current name supply.
isolate :: Query s a -> State GenState (GenState, a)
isolate (Query q) = do
    st <- get
    put $ (initState (nameScope st)) {nameSupply = nameSupply st}
    x <- q
    st' <- get
    put $ st {nameSupply = nameSupply st'}
    return (st', x)

-- | SQL generation internal state.
--   Contains the subqueries and static (i.e. not dependent on any subqueries)
--   restrictions of the query currently being built, as well as a name supply
--   for column renaming.
data GenState = GenState
    { sources         :: ![SQL]
    , staticRestricts :: ![Exp Bool]
    , groupCols       :: ![SomeCol]
    , nameSupply      :: !Int
    , nameScope       :: !Int
    }

-- | Initial state: no subqueries, no restrictions.
initState :: Int -> GenState
initState scope = GenState
    { sources = []
    , staticRestricts = []
    , groupCols = []
    , nameSupply = 0
    , nameScope  = scope
    }

renameAll :: [UntypedCol] -> State GenState [SomeCol]
renameAll = fmap concat . mapM rename

-- | Generate a unique name for the given column.
rename :: UntypedCol -> State GenState [SomeCol]
rename (Untyped col) = do
    n <- freshId
    return [Named (newName n) col]
  where
    newName ns =
        case col of
            Col n -> addColSuffix n $ "_" <> pack (show ns)
            _     -> mkColName $ "tmp_" <> pack (show ns)

-- | Get a guaranteed unique identifier.
freshId :: State GenState Name
freshId = do
    st <- get
    put $ st {nameSupply = succ $ nameSupply st}
    return (Name (nameScope st) (nameSupply st))

-- | Get a guaranteed unique column name.
freshName :: State GenState ColName
freshName = do
    n <- freshId
    return $ mkColName $ "tmp_" <> pack (show n)

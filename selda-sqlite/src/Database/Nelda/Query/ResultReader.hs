{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Nelda.Query.ResultReader where

import Database.Nelda.Backend.Types (SqlValue)
import Control.Monad.State (state, evalState, State)

newtype ResultReader a = ResultReader (State [SqlValue] a)
    deriving (Functor, Applicative, Monad)

runResultReader :: ResultReader a -> [SqlValue] -> a
runResultReader (ResultReader m) = evalState m

next :: ResultReader SqlValue
next = ResultReader . state $ \s -> (head s, tail s)

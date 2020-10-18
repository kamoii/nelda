-- | Functionality for inspecting and debugging Selda queries.
module Database.Selda.Debug
  ( OnError (..)
  , compile
  , compileDropTable
  , compileInsert, compileUpdate
  -- , compileCreateTable', compileCreateTable
  , compQuery, compQueryWithFreshScope
  ) where
import Database.Selda.Backend
import Database.Selda.Compile
import Database.Selda.Table
import Database.Selda.Table.Compile
import Database.Selda.SQL.Print.Config (ppConfig)
import Data.Text (Text)

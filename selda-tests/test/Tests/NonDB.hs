{-# LANGUAGE CPP #-}
-- | Misc. tests that don't touch the database.
module Tests.NonDB where
import Data.List hiding (groupBy, insert)
import Data.Text (unpack)
#ifdef POSTGRES
import Database.Selda.PostgreSQL
import Database.Selda.PostgreSQL.Debug (compile)
#endif
#ifdef SQLITE
import Database.Selda.SQLite
import Database.Selda.SQLite.Debug (compile)
#endif
import Test.HUnit
import Utils
import Tables

noDBTests = test
  [ "tableFieldMod modifies fields" ~: tfmModifiesFields
  ]

tfmModifiesFields =
  assertBool "Field names are unchanged from underlying record"
             ("mod_" `isInfixOf` q)
  where
    q = unpack $ fst $ compile (select modPeople)

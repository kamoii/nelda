{-# LANGUAGE NamedFieldPuns #-}

module Database.Nelda.Schema.TableConstraint where

import Database.Nelda.Schema.IsColumnNames (IsColumnNames(..))
import Database.Nelda.Schema.IsColumnSubset (IsColumnSubset)
import Database.Nelda.Schema.Table (Table(..), TableConstraint(..))

primaryKey
    :: ( IsColumnNames colNames
      , IsColumnSubset name cols (ToColumnNamesType colNames)
      )
    => colNames
    -> Table name cols
    -> Table name cols
primaryKey ts table@(Table {tabConstraints}) =
    table { tabConstraints = TCPrimaryKey (toColumnNames ts) : tabConstraints }

{-
-- Partial apply . Could not deduce
-- 型推論
foo :: _ => _
foo = primaryKey (#age, #hoge)

-- -- そうか
-- -- これが RankN多相で 型推論が死ぬってやつか...
-- -- こっちは曖昧な型
-- -- bar :: Show a => Int -> a -> Text
-- bar :: Int -> (forall a. Show a => a -> Text)
-- bar = undefined

-- bbb :: _ => _
-- bbb = bar 4

-- primaryKey :: ColumnName s -> PrimaryKey '[ColumnName s]
-- primaryKey (ColumnName pk) = PrimaryKey [pk]

-- primaryKey2 :: (ColumnName s0, ColumnName s1) -> PrimaryKey '[ColumnName s0, ColumnName s1]
-- primaryKey2 (ColumnName pk0, ColumnName pk1) = PrimaryKey [pk0, pk1]
-}

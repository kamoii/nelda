{-# LANGUAGE NamedFieldPuns #-}

module Database.Nelda.TabAttrDefinition where

import Database.Nelda.IsColNames (IsColNames(..))
import Database.Nelda.IsColumnSubset (IsColumnSubset)
import Database.Nelda.Table (Table(..))
import Database.Nelda.TabAttr

primaryKey
    :: ( IsColNames colNames
      , IsColumnSubset name cols (ToColNamesType colNames)
      )
    => colNames
    -> Table name cols
    -> Table name cols
primaryKey ts table@(Table {tabAttrs}) =
    table { tabAttrs = PrimaryKey (toAnyColNames ts) : tabAttrs }

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

-- primaryKey :: ColName s -> PrimaryKey '[ColName s]
-- primaryKey (ColName pk) = PrimaryKey [pk]

-- primaryKey2 :: (ColName s0, ColName s1) -> PrimaryKey '[ColName s0, ColName s1]
-- primaryKey2 (ColName pk0, ColName pk1) = PrimaryKey [pk0, pk1]
-}

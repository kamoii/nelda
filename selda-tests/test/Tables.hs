{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings, TypeOperators, DeriveGeneric, CPP #-}
#if MIN_VERSION_base(4, 9, 0)
{-# LANGUAGE OverloadedLabels, FlexibleContexts, DataKinds, MonoLocalBinds #-}
#endif
-- | Tables for reuse by most tests, and functions for their setup and teardown.
module Tables where
#ifdef POSTGRES
import Database.Selda.PostgreSQL
import Database.Selda.PostgreSQL.MakeSelectors
#endif

import Database.Nelda.Action
import Database.Nelda.Schema
import Database.Nelda.Backend.Monad (MonadNelda)
import qualified Database.Nelda.Schema.ColumnType as T
import Data.Text (Text)
import Data.Function ((&))
import JRec.Internal (fromNative)
import JRec
import GHC.Generics (Generic)

data Person = Person
    { name :: Text
    , age  :: Int
    , pet  :: Maybe Text
    , cash :: Double
    } deriving (Generic, Show, Ord, Eq)

-- modPeople :: Table Person
-- modPeople = tableFieldMod "modpeople" [Single pName :- primary] $ \name ->
--   "mod_" <> name

people = table #people
    ( column #name T.text   & notNull & primary
    , column #age  T.int    & notNull
    , column #pet  T.text
    , column #cash T.double & notNull
    )
    & addIndex (Single #name)
    & addIndex (Single #cash)
    & addIndex (#name, #cash)

-- people :: Table Person
-- people = table "people"
--   [ Single pName :- primary
--   , Single pName :- index
--   -- , Single pCash :- indexUsing HashIndex
--   -- SQLIte では indexUsing はないので
--   , Single pCash :- index
--   , pName :+ Single pCash :- index
--   ]

-- pName = #name :: Selector Person Text
-- pAge :: HasField "age" t => Selector t (FieldType "age" t)
-- pAge  = #age
-- pPet  = #pet  :: Selector Person (Maybe Text)
-- pCash = #cash :: HasField "cash" t => Selector t (FieldType "cash" t)

addresses = table #addresses
    ( column #name T.text & notNull
    , column #city T.text & notNull
    )
-- addresses :: Table (Text, Text)
-- (addresses, aName :*: aCity) = tableWithSelectors "addresses" []

comments = table #comments
    ( column #id      T.int & primary
    , column #name    T.text
    , column #comment T.text & notNull
    )

-- comments, weakComments :: Table (RowID, Maybe Text, Text)
-- comments = table "comments" [cId :- untypedAutoPrimary]
-- weakComments = table "comments" [cId :- weakUntypedAutoPrimary]
-- cId :*: cName :*: cComment = selectors comments

peopleItems =
    [ Person "Link"      125 (Just "horse")  13506
    , Person "Velvet"    19  Nothing         5.55
    , Person "Kobayashi" 23  (Just "dragon") 103707.55
    , Person "Miyu"      10  Nothing         (-500)
    ]

addressItems :: [Rec ["name" := Text, "city" := Text]]
addressItems =
    [ Rec (#name := "Link"      , #city := "Kakariko")
    , Rec (#name := "Kobayashi" , #city := "Tokyo")
    , Rec (#name := "Miyu"      , #city := "Fuyukishi")
    ]

commentItems :: [Rec ["name" := Maybe Text, "comment" := Text]]
commentItems =
    [ Rec (#name := Just "Link" , #comment := "Well, excuuuse me, princess!")
    , Rec (#name := Nothing     , #comment := "Anonymous spam comment")
    ]

resetup :: MonadNelda m => m ()
resetup = do
    createTableIfNotExists people
    -- createTableIfNotExists modPeople
    createTableIfNotExists addresses
    createTableIfNotExists comments

setup :: MonadNelda m => m ()
setup = do
    createTable people
    -- createTable modPeople
    createTable addresses
    createTable comments
    -- insert_ (modPeople) peopleItems
    insert_ people $ map fromNative peopleItems
    insert_ addresses addressItems
    insert_ comments commentItems

teardown :: MonadNelda m => m ()
teardown = do
    dropTableIfExists people
    -- dropTableIfExists modPeople
    dropTableIfExists addresses
    dropTableIfExists comments

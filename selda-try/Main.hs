{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Database.Selda.SQLite ((?), FieldType, Row, Col, (.>=), SqlRow, insert_, createTable, (!), restrict, (.==), withSQLite)
import qualified Database.Selda.SQLite as Selda
import Database.Selda.SQLite.Debug (compQuery)

import Database.Nelda.Schema
import Database.Nelda.Schema.ColumnType as T
import qualified Database.Nelda.Query.Select as Nelda
import qualified Database.Nelda.Action as Nelda

import Data.Function ((&))
import Text.Pretty.Simple
import GHC.Generics (Generic)
import Data.Text (Text)

import JRec
import GHC.Records.Compat (HasField)
import JRec.Internal (set, FldProxy(..), get, Has, Set)
import GHC.OverloadedLabels (IsLabel(fromLabel))

-- * HasField(record-hasfield) instance for Rec(jrec)

instance (Has l lts t, Set l lts t ~ lts) => HasField l (Rec lts) t where
    hasField r =
        ( \t -> set (FldProxy :: FldProxy l) t r
        , get (FldProxy :: FldProxy l) r
        )

-- * HasField(record-hasfield) instance for Row s a
--
-- Database.Selda.Selectors の (!) と (?) を考える必要がある。
--
-- Selector t a は OverloadedLabels で定義される(Database.Selda.FieldSelectors)
--
-- instance (Relational t, HasField name t, FieldType name t ~ a) =>
--      IsLabel name (S.Selector t a) where


instance {-# OVERLAPPABLE #-}
    ( Selda.Relational t, Selda.HasField name t, Selda.FieldType name t ~ a
    ) => HasField name (Row s t) (Col s a) where
    hasField row =
        ( error "set/modify is not supported for Row"
        , row ! fromLabel @name @(Selda.Selector t a)
        )

instance
    ( Selda.Relational t, Selda.HasField name t, Selda.FieldType name t ~ a
    , a' ~ Selda.Coalesce (Maybe a)
    ) => HasField name (Row s (Maybe t)) (Col s a') where
    hasField row =
        ( error "set/modify is not supported for Row"
        , row ? fromLabel @name @(Selda.Selector t a)
        )

{-
selda の以下のチュートリアルと同様レベルのものができるように
https://selda.link/tutorial/ch1-example-explained/

import Database.Selda
import Database.Selda.SQLite

data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

data Person = Person
  { name :: Text
  , age  :: Int
  , pet  :: Maybe Pet
  } deriving Generic
instance SqlRow Person

people :: Table Person
people = table "people" [#name :- primary]

main = withSQLite "people.sqlite" $ do
  createTable people
  insert_ people
    [ Person "Velvet"    19 (Just Dog)
    , Person "Kobayashi" 23 (Just Dragon)
    , Person "Miyu"      10 Nothing
    ]

  adultsAndTheirPets <- query $ do
    person <- select people
    restrict (person ! #age .>= 18)
    return (person ! #name :*: person ! #pet)
  liftIO $ print adultsAndTheirPets
-}

people = table #people
    ( column #name T.text & notNull
    , column #age T.int & notNull
    , column #pet T.text
    )

-- data Pet = Dog | Horse | Dragon
--   deriving (Show, Read, Bounded, Enum)
-- instance SqlType Pet

data Person = Person
  { name :: Text
  , age  :: Int
  , pet  :: Maybe Text
  } deriving Generic
instance SqlRow Person

people' :: Selda.Table Person
people' = Selda.table "people" []
-- people' = table "people" [#name :- primary]

-- bar :: Table (Int, Text)
-- bar = table "hoge" []

aaa :: Rec '[ "foo" := Int, "bar" := String]
aaa = Rec ( #foo := 12, #bar := "hoge")

test :: IO _
test = withSQLite "people.sqlite" $ do

    -- createTable people'

    -- insert_ people'
    --     [ Person "Velvet"    19 (Just "Dog")
    --     , Person "Kobayashi" 23 (Just "Dragon")
    --     , Person "Miyu"      10 Nothing
    --     ]
    -- Nelda.insert_ people
    --     -- [ Rec (#name := "bar", #age := 80, #pet := Nothing)
    --     [ Rec (#name := "Velvet", #age := 19, #pet := Just "Dog")
    --     , Rec (#name := "Kobayashi", #age := 23, #pet := Just "Dragon")
    --     , Rec (#name := "Miyu", #age := 10, #pet := Nothing)
    --     ]

    Selda.query $ do
        row <- Nelda.select people
        restrict $ row.age .>= 18
        pure row


-- TODO: pPrint の出力がコンパクトになるように調整したい
main :: IO ()
main = do
    pPrint $ people
    pPrint $ compQuery 0 $ do
        Nelda.select people
    pPrint $ compQuery 0 $ do
        row <- Nelda.select people
        restrict $ row ! #age .== 3
        pure row
    pPrint =<< test
    -- pPrint $ bar
    -- pPrint $ people
    -- pPrint $ compQuery 0 $ do
    --     row <- select people
    --     restrict $ row ! #age .== 3
    --     pure row
    -- pPrint $ compile 0 $ do
    --     selectValues [ Person "a" 4 Nothing]
    putStrLn "Hello, Haskell!"

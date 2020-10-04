{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Selda.SQLite
import Database.Selda.SQLite.Debug

-- import Database.Nelda.Schema
-- import Database.Nelda.Schema.ColumnType as T

import Data.Function ((&))
import Text.Pretty.Simple

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

-- people' = table #person
--     ( column #name T.text
--     , column #age T.int & notNull
--     , column #pet T.text
--     )

-- data Pet = Dog | Horse | Dragon
--   deriving (Show, Read, Bounded, Enum)
-- instance SqlType Pet

data Person = Person
  { name :: Text
  , age  :: Int
  , pet  :: Maybe Text
  } deriving Generic
instance SqlRow Person

people :: Table Person
people = table "people" [#name :- primary]

bar :: Table (Int, Text)
bar = table "hoge" []

main :: IO ()
main = do
    pPrint $ bar
    pPrint $ people
    pPrint $ compile $ do
        select people
        select people
    pPrint $ compile $ do
        selectValues [ Person "a" 4 Nothing]
    putStrLn "Hello, Haskell!"

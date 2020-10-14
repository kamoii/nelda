{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Selda.SQLite ((.>=), SqlRow, insert_, createTable, (!), restrict, (.==), withSQLite)
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

test :: IO _
test = withSQLite "people.sqlite" $ do

    -- createTable people'

    -- insert_ people'
    --     [ Person "Velvet"    19 (Just "Dog")
    --     , Person "Kobayashi" 23 (Just "Dragon")
    --     , Person "Miyu"      10 Nothing
    --     ]
    Nelda.insert_ people
        []
        -- [ Person "Velvet"    19 (Just "Dog")
        -- , Person "Kobayashi" 23 (Just "Dragon")
        -- , Person "Miyu"      10 Nothing
        -- ]

    Selda.query $ do
        row <- Nelda.select people
        restrict $ row ! #age .>= 18
        pure row
        -- person <- Selda.select people'
        -- restrict (person ! #age .>= 18)
        -- return (person ! #name :*: person ! #pet)


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

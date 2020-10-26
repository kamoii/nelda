{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
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
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Database.Nelda
import Database.Nelda.SQLite (withSQLite)
import Database.Nelda.Schema
import Database.Nelda.Schema.ColumnType as T
import Database.Nelda.Schema.ColumnConstraint
import Database.Nelda.SqlTypeDeriveStrategy as SqlTypeDeriving
import qualified Database.Nelda.Action as Nelda
import qualified Database.Nelda.Compile.Table as CreateTable
import qualified Database.Nelda.Compile.Index as CreateIndex

import Data.Function ((&))
import Text.Pretty.Simple
import Data.Text (Text)

import JRec
import GHC.Records.Compat (HasField)
import JRec.Internal (fromNative, set, FldProxy(..), get, Has, Set)
import GHC.OverloadedLabels (IsLabel(fromLabel))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Database.Nelda.SqlType (SqlType)
import qualified Database.Nelda.Query.SqlClause as Nelda
import Database.Nelda.Action (insert_, query)
import Database.Nelda.Query.SqlClause (restrict, select)
import Database.Nelda.Query.SqlExpression
import Database.Nelda.SQL.RowHasFieldInstance ()
import GHC.Generics (Generic)

-- * HasField(record-hasfield) instance for Rec(jrec)

instance (Has l lts t, Set l lts t ~ lts) => HasField l (Rec lts) t where
    hasField r =
        ( \t -> set (FldProxy :: FldProxy l) t r
        , get (FldProxy :: FldProxy l) r
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

-- data Person = Person
--     { name :: Text
--     , age  :: Int
--     , pet  :: Maybe Pet
--     } deriving Generic
-- instance SqlRow Person
--
-- people' :: Selda.Table Person
-- people' = Selda.table "people" []
-- people' = table "people" [#name :- primary]


data Pet = Dog | Horse | Dragon
    deriving (Show, Read, Bounded, Enum)
    deriving SqlType via SqlTypeDeriving.TextEnum Pet


people :: Table _ _
people = table #people
    ( column #name T.text & notNull & primary
    , column #age  T.int  & notNull
    , column #pet  (T.text & asSqlType @Pet) & default_ Dog
    )
    & addIndex (Single #name)

data People2 = People2
    { name :: Text
    , age :: Int
    } deriving Generic

test :: IO _
test = withSQLite "people.sqlite" $ do

    -- liftIO $ print $ CreateTable.compileCreateTable CreateTable.defaultConfig people
    -- let Table{tabIndexies} = people
    -- liftIO $ print $ map (CreateIndex.compileCreateIndex CreateIndex.defaultConfig) tabIndexies
    -- createTable people

    insert_ people
        [ Rec (#name := ("Velvet" :: Text), #age := (19 :: Int), #pet := Just Dog)
        , Rec (#name := "Kobayashi", #age := 23, #pet := Just Dragon)
        , Rec (#name := "Miyu",      #age := 10, #pet := Nothing)
        ]
    insert_ people $ map fromNative
        [ People2 "foo" 23 ]

    query $ do
        row <- select people
        restrict $ row.age .>= 18
        pure $ row.age :*: row.name

-- TODO: pPrint の出力がコンパクトになるように調整したい
main :: IO ()
main = do
    -- pPrint $ people
    -- pPrint $ compQuery 0 $ do
    --     Nelda.select people
    -- pPrint $ compQuery 0 $ do
    --     row <- Nelda.select people
    --     restrict $ row ! #age .== 3
    --     pure row
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

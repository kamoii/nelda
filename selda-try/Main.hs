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

import Database.Nelda.SQLite (withSQLite)
import Database.Nelda.Schema
import Database.Nelda.Schema.ColumnType as T ( int, text )
-- import Database.Nelda.Schema.ColumnConstraint
import Database.Nelda.SqlTypeDeriveStrategy as SqlTypeDeriving
    ( TextEnum(..) )
-- import qualified Database.Nelda.Action as Nelda
-- import qualified Database.Nelda.Compile.Table as CreateTable
-- import qualified Database.Nelda.Compile.Index as CreateIndex

import Data.Function ((&))
import Text.Pretty.Simple
import Data.Text (Text)

import JRec
import GHC.Records.Compat (HasField)
import JRec.Internal (set, FldProxy(..), get, Has, Set)
import GHC.OverloadedLabels (IsLabel(fromLabel))
import Database.Nelda.SqlType (SqlType)
import Database.Nelda.Action (query)
import Database.Nelda.Query.SqlClause (values, restrict, select)
import Database.Nelda.Query.SqlExpression
import Database.Nelda.SQL.RowHasFieldInstance ()
import GHC.Generics (Generic)

-- * HasField(record-hasfield) instance for Rec(jrec)

instance (Has l lts t, Set l lts t ~ lts) => HasField l (Rec lts) t where
    hasField r =
        ( \t -> set (FldProxy :: FldProxy l) t r
        , get (FldProxy :: FldProxy l) r
        )

-- | Evil use of HasField/RecordDotSyntax
--
-- With this instance, all functions that recieves OverloadedLabels as first arugment
-- can accept their first argument thourgh RecordDotSyntax.
--
-- For example:
--
--   column #age intType
--
-- could be written
--
--   column.age intType
--
-- NOTE: You shouldn't use this.
-- Use only if you can't bear the syntax noiseness of OverloadedLabels.
instance (IsLabel label a, b ~ b') => HasField label ((->) a b) b' where
    hasField f =
        ( \b -> const b <$> f      -- Type-checks. But does it make sense?
        , f (fromLabel @label @a)
        )

-- | One more evil case.
-- instance (HasField label a b, b' ~ Maybe b) => HasField label (Mabye a) b'

-- * Sample

data Pet = Dog | Horse | Dragon
    deriving (Show, Read, Bounded, Enum)
    deriving SqlType via SqlTypeDeriving.TextEnum Pet


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

test = withSQLite "people.sqlite" $ do

    -- liftIO $ print $ CreateTable.compileCreateTable CreateTable.defaultConfig people
    -- let Table{tabIndexies} = people
    -- liftIO $ print $ map (CreateIndex.compileCreateIndex CreateIndex.defaultConfig) tabIndexies
    -- createTable people

    -- insert_ people
    --     [ Rec (#name := ("Velvet" :: Text), #age := (19 :: Int), #pet := Just Dog)
    --     , Rec (#name := "Kobayashi", #age := 23, #pet := Just Dragon)
    --     , Rec (#name := "Miyu",      #age := 10, #pet := Nothing)
    --     ]
    -- insert_ people $ map fromNative
    --     [ People2 "foo" 23 ]

    query $ do
        row <- select people
        -- val <- values ([] :: [Rec '["hoge" := Pet]])
        restrict $ row.age .>= 18
        pure (row.age, row.name)

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

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Database.Nelda.Schema
import Database.Nelda.Schema.ColumnType as T
-- import Database.Nelda.SqlType
import Data.Function ((&))


foo :: _
foo = table #foo
    ( column #age T.int & notNull
    , column #name T.text
    , column #a T.text
    )

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"

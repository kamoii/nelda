{-# LANGUAGE OverloadedLabels #-}

module Main where

import Database.Nelda.Schema
import Database.Nelda.Schema.SqlColumnType
import Database.Nelda.SqlType

foo = table #foo
    ( column #hoge int
    , column #hoge text
    )

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"

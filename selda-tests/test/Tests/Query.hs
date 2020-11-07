{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

-- | Tests that don't modify the database.
module Tests.Query (queryTests) where

import qualified Data.Char as Char (toUpper)
import Data.Function (on)
import Data.List hiding (groupBy, insert, union)
import Data.Maybe (catMaybes)

#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup (Semigroup (..))
#endif
import qualified Data.Text as Text (map, toUpper)

#ifdef POSTGRES
import Database.Selda.PostgreSQL as Selda hiding (on)
import Database.Selda.PostgreSQL.Nullable
import Database.Selda.PostgreSQL.Unsafe
import Database.Selda.PostgreSQL.Validation
#endif
#ifdef SQLITE
#endif

import Data.Data (Proxy (Proxy))
import Data.Text (Text)
import Data.Tup ((:*:) ((:*:)))
import Database.Nelda.Action (deleteFrom_, insertFromNative_, query)
import Database.Nelda.Backend.Monad (NeldaM)
import Database.Nelda.Query.SqlClause (aggregate, ascending, descending, distinct, groupBy, innerJoin, leftJoin, leftJoin', limit, nonNull, order, restrict, restrict', select, suchThat, union, values, valuesAsCol, valuesFromNative)
import qualified Database.Nelda.Query.SqlClauseUnsafe as Unsafe
import Database.Nelda.Query.SqlExpression (avg_, count_, div_, false_, float_, if_, int_, isIn_, isNull_, length_, matchNull_, max_, min_, null_, round_, sum_, text_, toNullable, true_, (.*), (.+), (.-), (./), (./=), (.<), (.<=), (.==), (.>), (.>=))
import qualified Database.Nelda.Query.SqlExpression as Nelda
import qualified Database.Nelda.Query.SqlExpressionUnsafe as Unsafe
import Database.Nelda.SQL.Col (Col)
import Database.Nelda.SQL.Nullability
import Database.Nelda.SQL.Row (C, CS, (:-))
import Database.Nelda.SQL.RowHasFieldInstance ()
import qualified Database.Nelda.Schema.ColumnType as CT
import Database.Nelda.SqlType (SqlType)
import GHC.Generics (Generic)
import JRec hiding (union)
import qualified JRec.Internal as JRec
import Tables
import Test.HUnit
import Utils

queryTests :: (NeldaM () -> IO ()) -> Test
queryTests run =
    test
        [ "setup succeeds" ~: run setup
        , "simple select" ~: run simpleSelect
        , "simple product" ~: run simpleProduct
        , "order ascending" ~: run orderAscending
        , "filter equal" ~: run filterEqual
        , "filter not equal" ~: run filterNotEqual
        , "join-like product" ~: run joinLikeProduct
        , "join-like product with sels" ~: run joinLikeProductWithSels
        , "simple left join" ~: run simpleLeftJoin
        , "row left join" ~: run rowLeftJoin
        , "left join followed by product" ~: run leftJoinThenProduct
        , "count_ aggregation" ~: run countAggregate
        , "aggregate with join and group" ~: run joinGroupAggregate
        , "nested left join" ~: run nestedLeftJoin
        , "order + limit" ~: run orderLimit
        , "limit gives correct number of results" ~: run limitCorrectNumber
        , "aggregate with doubles" ~: run aggregateWithDoubles
        , "select from value table" ~: run selectVals
        , "select from empty value table" ~: run selectEmptyValues
        , "aggregate from empty value table" ~: run aggregateEmptyValues
        , "inner join" ~: run testInnerJoin
        , "simple if-then-else" ~: run simpleIfThenElse
        , "rounding doubles to int_s" ~: run roundToInt
        , "serializing doubles" ~: run serializeDouble
        , "such that works" ~: run testSuchThat
        , -- , "prepared without args" ~: run preparedNoArgs
          -- , "prepared with args" ~: run preparedManyArgs
          -- , "prepared int_erleaved" ~: run preparedInterleaved
          -- , "int_erleaved with different results" ~: run preparedDifferentResults
          "order in correct order" ~: run orderCorrectOrder
        , "multiple aggregates in sequence (#42)" ~: run multipleAggregates
        , "isIn_ inner query renaming (#46)" ~: run isInQueryRenaming
        , "distinct on multiple queries" ~: run selectDistinct
        , "distinct on single query" ~: run selectValuesDistinct
        , "distinct restrict" ~: run selectRestrictedDistinct
        , "matchNull_" ~: run simpleMatchNull
        , -- , "if_" ~: run simpleIfThenElse
          -- , -- , "validateTable validates"            ~: run validateTableValidates
          "aggregate empty table" ~: run aggregateEmptyTable
        , "empty singleton values" ~: run selectValuesEmptySingletonTable
        , "coalesce row" ~: run coalesceRow
        , "coalesce equality" ~: run coalesceEquality
        , "coalesce num" ~: run coalesceNum
        , "coalesce frac" ~: run coalesceFrac
        , "coalesce sum" ~: run coalesceSum
        , "nonNull" ~: run nonNullYieldsEmptyResult
        , "rawQuery1" ~: run rawQuery1Works
        , "rawQuery" ~: run rawQueryWorks
        , "union" ~: run unionWorks
        , "union discards dupes" ~: run unionDiscardsDupes
        , "union works for whole rows" ~: run unionWorksForWholeRows
        , -- , "expression cols under union (LHS)" ~: run unionWithLhsExpressionCols
          -- , "expression cols under union (RHS)" ~: run unionWithRhsExpressionCols
          -- , "unionAll" ~: run unionAllWorks
          -- , "unionAll works for whole rows" ~: run unionAllForWholeRows
          -- , "string concatenation" ~: run stringConcatenation
          -- , "teardown succeeds" ~: run teardown
          -- , "if not exists works" ~: run (setup >> resetup)
          "nullable operands" ~: run nullableOperands
        ]

nullableOperands = do
    -- そもそもが型が Nullable になる必要がある
    [b] <- query $ pure $ 4 .== null_ @Int
    assEq "wrong results from select" Nothing b

simpleSelect = do
    ppl <- query $ select people
    assEq "wrong results from select" (sort peopleItems) (sort ppl)

simpleProduct = do
    prod <- query $ do
        a <- select addresses
        p <- select people
        pure (a.name, a.city, p)
    assEq "wrong results from product" (sort ans) (sort prod)
  where
    ans = [(a.name, a.city, p) | p <- peopleItems, a <- addressItems]

orderAscending = do
    ppl <- query $ do
        p <- select people
        order (p.name) ascending
        return p
    assEq "result not properly sorted" (sort peopleItems) ppl

filterEqual = do
    ppl <- query $ do
        p <- select people
        restrict (p.name .== "Link")
        pure p.name
    assEq "unequal elements not removed" ["Link"] ppl

filterNotEqual = do
    ppl <- query $ do
        p <- select people
        restrict (p.name ./= "Link")
        return p.name
    ass "filtered element still in list" (not $ "Link" `elem` ppl)

joinLikeProduct = do
    res <- query $ do
        name <- (.name) <$> select people
        a <- select addresses
        restrict (name .== a.name)
        return (name, a.city)
    assEq "join-like query gave wrong result" (sort ans) (sort res)
  where
    ans =
        [ (p.name, c)
        | p <- peopleItems
        , Rec (_ := n', _ := c) <- addressItems
        , p.name == n'
        ]

joinLikeProductWithSels = do
    res <- query $ do
        p <- select people
        a <- select addresses
        restrict (p.name .== a.name)
        return (p.name, a.city, p.pet)
    assEq "join-like query gave wrong result" (sort ans) (sort res)
  where
    ans =
        [ (n, c, p)
        | Person n _ p _ <- map JRec.toNative peopleItems
        , Rec (_ := n', _ := c) <- addressItems
        , n == n'
        ]

simpleLeftJoin = do
    res <- query $ do
        name <- (.name) <$> select people
        a <-
            leftJoin
                (\a -> name .== a.name)
                (select addresses)
        return (name, a.city)
    assEq "join-like query gave wrong result" (sort ans) (sort res)
  where
    ans =
        [ ("Link", Just "Kakariko")
        , ("Velvet", Nothing)
        , ("Miyu", Just "Fuyukishi")
        , ("Kobayashi", Just "Tokyo")
        ]

rowLeftJoin = do
    res <- query $ do
        name <- (.name) <$> select people
        a <-
            leftJoin
                (\a -> name .== a.name)
                (select addresses)
        return (name, a)
    assEq "join-like query gave wrong result" (sort ans) (sort res)
  where
    ans =
        [ ("Link", Just (Rec (#name := "Link", #city := "Kakariko")))
        , ("Velvet", Nothing)
        , ("Miyu", Just (Rec (#name := "Miyu", #city := "Fuyukishi")))
        , ("Kobayashi", Just (Rec (#name := "Kobayashi", #city := "Tokyo")))
        ]

leftJoinThenProduct = do
    res <- query $ do
        name <- (.name) <$> select people
        a <-
            leftJoin
                (\a -> name .== a.name)
                (select addresses)
        c <- select comments
        restrict' $ c.name .== name
        return (name, a.city, c.comment)
    assEq "join + product gave wrong result" ans res
  where
    linkComment = head [c | Rec (_ := n, _ := c) <- commentItems, n == Just "Link"]
    ans = [("Link", Just "Kakariko", linkComment)]

countAggregate = do
    [res] <- query . aggregate $ do
        p <- select people
        return $ count_ p.pet
    assEq "count_ counted the wrong number of pets" ans res
  where
    ans = length [p | Just p <- map (.pet) peopleItems]

joinGroupAggregate = do
    res <- query . aggregate $ do
        p <- select people
        a <-
            leftJoin
                (\a -> p.name .== a.name)
                (select addresses)
        nopet <- groupBy (isNull_ p.pet)
        return (nopet, count_ a.city)
    assEq "wrong number of cities per pet owneship status" ans (sort res)
  where
    -- There are pet owners in Tokyo and Kakariko, there is no pet owner in
    -- Fuyukishi
    ans = [(False, 2), (True, 1)]

nestedLeftJoin = do
    res <- query $ do
        p <- select people
        (_, city, cs) <- leftJoin (\(name', _, _) -> p.name .== name') $ do
            a <- select addresses
            (_, cs) <- leftJoin' (\(n, _) -> n .== a.name) $ do
                (n, cs) <- aggregate $ do
                    c <- select comments
                    n <- groupBy c.name
                    return (n, count_ c.comment)
                pure (n, cs)
            return (a.name, a.city, cs)
        return (p.name, city, cs)
    ass ("user with comment not in result: " ++ show res) (link `elem` res)
    ass ("user without comment not in result: " ++ show res) (velvet `elem` res)
  where
    link = ("Link", Just "Kakariko", Just (1 :: Int))
    velvet = ("Velvet", Nothing, Nothing)

orderLimit = do
    res <- query $
        limit 1 2 $ do
            t <- select people
            order t.cash descending
            return t.name
    assEq "got wrong result" ["Link", "Velvet"] (sort res)

limitCorrectNumber = do
    res <- query $ do
        p1 <- limit 1 2 $ select people
        _p2 <- limit 1 2 $ select people
        return p1
    assEq ("wrong number of results from limit") 4 (length res)

aggregateWithDoubles = do
    [Just res] <- query $
        aggregate $ do
            cash <- (.cash) <$> select people
            return $ avg_ cash
    assEq "got wrong result" ans res
  where
    ans = sum (map (.cash) peopleItems) / fromIntegral (length peopleItems)

selectVals = do
    vals <- query $ values peopleItems
    assEq "wrong columns returned" (sort peopleItems) (sort vals)

selectEmptyValues = do
    res <- query $ do
        _ppl <- select people
        _vals <- values ([] :: [Rec '["foo" := Maybe Text]])
        cs <- select comments
        pure cs
    assEq "result set wasn't empty" [] res

aggregateEmptyValues = do
    [res] <- query $
        aggregate $ do
            _ppl <- select people
            _vals <- values ([] :: [Rec '[]])
            t <- select comments
            return $ count_ t.id
    assEq "wrong count_ for empty result set" 0 res

testInnerJoin = do
    res <- query $ do
        p <- select people
        a <- innerJoin (\a -> p.name .== a.name) $ do
            select addresses
        return (p.pet, a.city)
    assEq "wrong result" oracle res
  where
    oracle =
        [ (Just "horse", "Kakariko")
        , (Just "dragon", "Tokyo")
        , (Nothing, "Fuyukishi")
        ]

simpleIfThenElse = do
    ppl <- query $ do
        t <- select people
        let ageGroup =
                if_ (t.age .< 18) (text_ "Child") $
                    if_
                        (t.age .>= 65)
                        (text_ "Elder")
                        (text_ "Adult")
        return (t.name, t.age, ageGroup)
    assEq "wrong results from if_" (sort res) (sort ppl)
  where
    res =
        [ ("Link", 125, "Elder")
        , ("Velvet", 19, "Adult")
        , ("Kobayashi", 23, "Adult")
        , ("Miyu", 10, "Child")
        ]

roundToInt = do
    res <- query $ do
        val <- valuesAsCol [1.1, 1.5, 1.9]
        return $ Unsafe.cast CT.int $ round_ val
    assEq "bad rounding" [1, 2, 2 :: Int] res

serializeDouble = do
    -- The "protocol" used by PostgreSQL is insane - better check that we speak
    -- it properly!
    res <- query $ do
        n <- valuesAsCol [123456789 :: Int]
        d <- valuesAsCol [123456789.3 :: Double]
        restrict (d .> Unsafe.cast CT.double n)
        return $ Unsafe.cast CT.double n + float_ 1.123
    assEq "wrong encoding" 1 (length res)
    assEq "wrong decoding" [123456790.123] res

testSuchThat = do
    res <- query $ do
        n1 <- ((.name) <$> select people) `suchThat` (.== "Link")
        n2 <- ((.name) <$> select people) `suchThat` (.== "Velvet")
        return (n1, n2)
    assEq "got wrong result" [("Link", "Velvet")] res

-- PREPARED は未対応なので...

-- {-# NOINLINE allShortNames #-}
-- allShortNames :: NeldaM [Text_]
-- allShortNames = prepared $ do
--     p <- select people
--     restrict (length_ (p ! pName) .<= 4)
--     order (p ! pName) ascending
--     return (p ! pName)

-- preparedNoArgs = do
--     res1 <- allShortNames
--     res2 <- allShortNames
--     res3 <- allShortNames
--     assEq "got wrong result" res res1
--     ass "subsequent calls gave different results" (all (== res1) [res2, res3])
--   where
--     res = ["Link", "Miyu"]

-- {-# NOINLINE allNamesLike #-}
-- -- Extra restricts to force the presence of a few non-argument parameters.
-- allNamesLike :: Int -> Text_ -> NeldaM [Text]
-- allNamesLike = prepared $ \len s -> do
--     p <- select people
--     restrict (length_ (p ! pName) .> 0)
--     restrict (p ! pName `like` s)
--     restrict (length_ (p ! pName) .> 1)
--     restrict (length_ (p ! pName) .<= len)
--     restrict (length_ (p ! pName) .<= 100)
--     restrict (length_ (p ! pName) .<= 200)
--     order (p ! pName) ascending
--     return (p ! pName)

-- preparedManyArgs = do
--     res1 <- allNamesLike 4 "%y%"
--     res2 <- allNamesLike 5 "%y%"
--     res3 <- allNamesLike 6 "%y%"
--     assEq "got wrong result" res res1
--     ass "subsequent calls gave different results" (all (== res1) [res2, res3])
--   where
--     res = ["Miyu"]

-- preparedInterleaved = do
--     asn1 <- allShortNames
--     anl1 <- allNamesLike 4 "%y%"
--     asn2 <- allShortNames
--     anl2 <- allNamesLike 5 "%y%"
--     asn3 <- allShortNames
--     anl3 <- allNamesLike 6 "%y%"
--     assEq "wrong result from allShortNames" asnres asn1
--     assEq "wrong result from allNamesLike" anlres anl1
--     ass
--         "subsequent allShortNames calls gave different results"
--         (all (== asn1) [asn2, asn3])
--     ass
--         "subsequent allNamesLike calls gave different results"
--         (all (== anl1) [anl2, anl3])
--   where
--     asnres = ["Link", "Miyu"]
--     anlres = ["Miyu"]

-- preparedDifferentResults = do
--     res1 <- allNamesLike 4 "%y%"
--     res2 <- allNamesLike 10 "%y%"
--     assEq "wrong result from first query" ["Miyu"] res1
--     assEq "wrong result from second query" ["Kobayashi", "Miyu"] res2

orderCorrectOrder = do
    insertFromNative_ people [Person "Amber" 19 Nothing 123]

    res1 <- query $ do
        p <- select people
        order p.name ascending
        order p.age ascending
        return p.name

    res2 <- query $ do
        p <- select people
        order p.name descending
        order p.age ascending
        return p.name

    res3 <- query $ do
        p <- select people
        order p.age descending
        order p.name ascending
        return p.name

    deleteFrom_ people $ \p -> p.name .== "Amber"

    assEq "latest ordering did not take precedence in first query" ans1 res1
    assEq "latest ordering did not take precedence in second query" ans2 res2
    assEq "latest ordering did not take precedence in third query" ans3 res3
  where
    ans1 = ["Miyu", "Amber", "Velvet", "Kobayashi", "Link"]
    ans2 = ["Miyu", "Velvet", "Amber", "Kobayashi", "Link"]
    ans3 = ["Amber", "Kobayashi", "Link", "Miyu", "Velvet"]

-- Test case for #42: name supply was erroneously overwritten when using
-- aggregates.
multipleAggregates = do
    res <- query $ do
        p <- select people

        (owner, homes) <- aggregate $ do
            a <- select addresses
            owner' <- groupBy a.name
            return (owner', count_ a.city)
        restrict (owner .== p.name)

        (owner2, homesInT_okyo) <- aggregate $ do
            a <- select addresses
            restrict (a.city .== "Tokyo")
            owner' <- groupBy a.name
            return (owner', count_ a.city)
        restrict (owner2 .== p.name)

        order homes descending
        return (owner, homes, homesInT_okyo)
    assEq "wrong result for aggregate query" [("Kobayashi", 1, 1)] res

isInQueryRenaming = do
    res <- query $ do
        t1 <- select people
        restrict $
            (int_ 1)
                `isIn_` ( do
                            t2 <- select people
                            t3 <- select addresses
                            restrict (t3.name .== t2.name)
                            restrict (t1.name .== t2.name)
                            restrict (t3.city .== "Kakariko")
                            return (int_ 1)
                        )
        return t1.name
    assEq "wrong list of people returned" ["Link"] res

selectDistinct = do
    res <- query $
        distinct $ do
            t <- select people
            select people
            order t.name ascending
            return t.name
    assEq "wrong result set" ["Kobayashi", "Link", "Miyu", "Velvet"] res

newtype L = L Text
    deriving (Generic, Show, Eq)
    deriving (SqlType) via Text

selectValuesDistinct = do
    res <- query $ distinct $ valuesAsCol $ replicate 5 (L "Link")
    assEq "wrong result set" [L "Link"] res

data Distinct = D {a :: Int, b :: Int}
    deriving (Generic)

selectRestrictedDistinct = do
    xs <- query $
        distinct $ do
            x <- valuesFromNative [D 42 1, D 42 2, D 42 3]
            restrict $ x.b .>= 2
            return $ x.a
    assEq "wrong result set" [42] xs

simpleMatchNull = do
    res <- query $ do
        t <- select people
        order t.name ascending
        return $ (t.name, matchNull_ 0 length_ t.pet)
    assEq "wrong result set" expected res
  where
    expected =
        [ ("Kobayashi", 6)
        , ("Link", 5)
        , ("Miyu", 0)
        , ("Velvet", 0)
        ]

-- validateTableValidates = do
--     validateTable people
--     assertFail $ validateTable bad
--   where
--     bad :: Table _name (RowID, RowID)
--     bad =
--         table
--             "bad"
--             [ Single (unsafeSelector 0 :: Selector (RowID, RowID) RowID) :- primary
--             , Single (unsafeSelector 1 :: Selector (RowID, RowID) RowID) :- primary
--             ]

aggregateEmptyTable = do
    [res] <- query $ do
        aggregate $ count_ <$> (.a) <$> values ([] :: [Rec ["a" := Int, "b" := Int]])
    assEq "count_ of empty table not 0" (0 :: Int) res

    [res] <- query $ do
        aggregate $ sum_ <$> (.a) <$> values ([] :: [Rec ["a" := Int, "b" := Int]])
    (res :: Int) `seq` return ()
    assEq "sum of empty table not 0" (0 :: Int) res

    [res] <- query $ do
        aggregate $ avg_ <$> (.a) <$> values ([] :: [Rec ["a" := Int, "b" := Int]])
    assEq "average of empty table not Nothing" (Nothing :: Maybe Int) res

    [res] <- query $ do
        aggregate $ min_ <$> (.a) <$> values ([] :: [Rec ["a" := Int, "b" := Int]])
    assEq "min of empty table not Nothing" (Nothing :: Maybe Int) res

    [res] <- query $ do
        aggregate $ max_ <$> (.a) <$> values ([] :: [Rec ["a" := Int, "b" := Int]])
    assEq "max of empty table not Nothing" (Nothing :: Maybe Int) res

selectValuesEmptySingletonTable = do
    [res] <-
        query $
            aggregate $
                count_ <$> valuesAsCol ([] :: [Int])
    assEq "non-zero count_ when selecting from empty values" 0 res

coalesceRow = do
    empty <- query $ do
        _ <- valuesAsCol [(1 :: Int)]
        xs <- leftJoin (const false_) (select people)
        return xs.name
    assEq "result not single null" [Nothing] empty

    res <- query $ do
        person <- select people
        restrict $ person.name .== "Link"
        xs <- leftJoin (const true_) (select people)
        order xs.name ascending
        return $ xs.name
    assEq "wrong result after coalescing" expected res
  where
    expected =
        [ Just "Kobayashi"
        , Just "Link"
        , Just "Miyu"
        , Just "Velvet"
        ]

coalesceEquality :: NeldaM ()
coalesceEquality = do
    ["Link"] <- query $ do
        person <- select people
        restrict' $ person.pet .== text_ "horse"
        return $ person.name
    ["Kobayashi"] <- query $ do
        person <- select people
        restrict' $ person.pet ./= text_ "horse"
        return person.name
    return ()

coalesceNum :: NeldaM ()
coalesceNum = do
    [(Just 250, Just 126, Just 124)] <- query $ do
        _ <- valuesAsCol [(1 :: Int)]
        person <- leftJoin (const true_) (select people)
        restrict' $ person.name .== text_ "Link"
        return
            ( person.age .* int_ 2
            , person.age .+ int_ 1
            , person.age .- int_ 1
            )
    return ()

coalesceFrac = do
    result <- query $ do
        _ <- valuesAsCol [(1 :: Int)]
        person <- leftJoin (const true_) (select people)
        restrict' $ person.name .== text_ "Miyu"
        return (person.cash ./ float_ 2, person.age `div_` int_ 2)
    assEq "wrong calculation results" [(Just (-250), Just 5)] result

coalesceSum = do
    res <- query $
        aggregate $ do
            _ <- valuesAsCol [(1 :: Int)]
            person <- leftJoin (const true_) (select people)
            age <- nonNull $ person.age
            return $ sum_ age
    assEq "wrong sum" [125 + 19 + 23 + 10 :: Int] res

nonNullYieldsEmptyResult = do
    empty <- query $ do
        _ <- valuesAsCol [(1 :: Int)]
        person <- leftJoin (const false_) (select people)
        nonNull $ person.age
    assEq "empty list not empty" [] empty

rawQuery1Works = do
    names <- query $ do
        n <- Unsafe.rawQuery1 (Proxy @'("foo", 'NonNull, Text)) "SELECT name as foo FROM people"
        order n ascending
        return n
    assEq "wrong name list returned" (sort $ map (.name) peopleItems) names

rawQueryWorks = do
    ppl <- query $ do
        p <-
            Unsafe.rawQuery
                ( Proxy
                    @( CS
                        '[ "name" :- C 'NonNull Text
                         , "age" :- C 'NonNull Int
                         , "pet" :- C 'Nullable Text
                         , "cash" :- C 'NonNull Double
                         ]
                     )
                )
                ("SELECT * FROM people WHERE name = " <> Unsafe.injectLiteral ("Link" :: Text))
        return p
    let correct = [p | p <- peopleItems, p.name == "Link"]
    assEq "wrong name list returned" correct ppl

unionWorks = assQueryEq "wrong name list returned" correct $ do
    let ppl = (.name) <$> select people
        pets = ((.pet) <$> select people) >>= nonNull
    name <- Nelda.toUpper_ <$> union pets ppl
    order name ascending
    return name
  where
    correct =
        sort $
            map Text.toUpper $
                map (.name) peopleItems ++ catMaybes (map (.pet) peopleItems)

unionDiscardsDupes = assQueryEq "wrong name list returned" correct $ do
    let ppl = (.name) <$> select people
        pets = ((.pet) <$> select people) >>= nonNull
    name <- Nelda.toUpper_ <$> ppl `union` pets `union` ppl `union` ppl
    order name ascending
    return name
  where
    correct =
        sort $
            map Text.toUpper $
                map (.name) peopleItems
                    ++ catMaybes (map (.pet) peopleItems)

unionWorksForWholeRows = assQueryEq "wrong person list returned" correct $ do
    let ppl1 = select people `suchThat` \p -> p.age .<= 18
        ppl2 = select people `suchThat` \p -> p.age .> 18
    ppl <- ppl1 `union` ppl2 `union` ppl2 `union` ppl1
    order (Nelda.toUpper_ ppl.name) ascending
    return ppl
  where
    correct = sortBy (compare `on` (Text.toUpper . (.name))) peopleItems

-- unionWithLhsExpressionCols = assQueryEq "wrong person list returned" correct $ do
--     let ppl1 = select people
--         ppl2 = (`with` [pAge += 1]) <$> select people
--     ppl <- ppl2 `union` ppl1
--     order ppl.age ascending
--     return ppl
--   where
--     correct = sortBy (compare `on` age) (peopleItems ++ map (\p -> p{age = age p + 1}) peopleItems)

-- unionWithRhsExpressionCols = assQueryEq "wrong person list returned" correct $ do
--     let ppl1 = select people
--         ppl2 = (`with` [pAge += 1]) <$> select people
--     ppl <- ppl1 `union` ppl2
--     order (ppl ! pAge) ascending
--     return ppl
--   where
--     correct = sortBy (compare `on` age) (peopleItems ++ map (\p -> p{age = age p + 1}) peopleItems)

-- unionAllWorks = assQueryEq "wrong name list returned" correct $ do
--     let ppl = pName `from` select people
--         pets = (pPet `from` select people) >>= nonNull
--     name <- Nelda.toUpper_ <$> ppl `unionAll` pets `unionAll` ppl
--     order name ascending
--     return name
--   where
--     correct =
--         sort $
--             map (Text.map Char.toUpper) $
--                 map name peopleItems
--                     ++ map name peopleItems
--                     ++ catMaybes (map pet peopleItems)

-- unionAllForWholeRows = assQueryEq "wrong person list returned" correct $ do
--     let ppl1 = select people
--         ppl2 = (`with` [pAge += 1]) <$> select people
--     ppl <- ppl1 `unionAll` ppl2
--     order (ppl ! pAge) ascending
--     return ppl
--   where
--     correct = sortBy (compare `on` age) (peopleItems ++ map (\p -> p{age = age p + 1}) peopleItems)

-- stringConcatenation = assQueryEq "wrong string returned" ["abcde"] $ do
--     pure $ mconcat ["a" :: Col s 'NonNull Text, "bc", "", "de"]

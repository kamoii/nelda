{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text as Text (map)

#ifdef POSTGRES
import Database.Selda.PostgreSQL as Selda hiding (on)
import Database.Selda.PostgreSQL.Nullable
import Database.Selda.PostgreSQL.Unsafe
import Database.Selda.PostgreSQL.Validation
#endif
#ifdef SQLITE
#endif

import Data.Text (Text)
import Data.Tup ((:*:) ((:*:)))
import Database.Nelda.Action (query)
import Database.Nelda.Backend.Monad (NeldaM)
import Database.Nelda.Query.SqlClause (aggregate, ascending, leftJoin, order, restrict, select, values)
import Database.Nelda.Query.SqlExpression (just, (./=), (.==))
import qualified Database.Nelda.Query.SqlExpression as Nelda
import Database.Nelda.SQL.Col (Col)
import Database.Nelda.SQL.RowHasFieldInstance ()
import GHC.Generics (Generic)
import JRec
import JRec.Internal (toNative)
import Tables
import Test.HUnit
import Utils
import Database.Nelda.Query.SqlExpression (count)

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
        , "count aggregation" ~: run countAggregate
        -- , "aggregate with join and group" ~: run joinGroupAggregate
        -- , "nested left join" ~: run nestedLeftJoin
        -- , "order + limit" ~: run orderLimit
        -- , "limit gives correct number of results" ~: run limitCorrectNumber
        -- , "aggregate with doubles" ~: run aggregateWithDoubles
        -- , "select from value table" ~: run selectVals
        -- , "select from empty value table" ~: run selectEmptyValues
        -- , "aggregate from empty value table" ~: run aggregateEmptyValues
        -- , "inner join" ~: run testInnerJoin
        -- , "simple if-then-else" ~: run simpleIfThenElse
        -- , "rounding doubles to ints" ~: run roundToInt
        -- , "serializing doubles" ~: run serializeDouble
        -- , "such that works" ~: run testSuchThat
        -- , "prepared without args" ~: run preparedNoArgs
        -- , "prepared with args" ~: run preparedManyArgs
        -- , "prepared interleaved" ~: run preparedInterleaved
        -- , "interleaved with different results" ~: run preparedDifferentResults
        -- , "order in correct order" ~: run orderCorrectOrder
        -- , "multiple aggregates in sequence (#42)" ~: run multipleAggregates
        -- , "isIn inner query renaming (#46)" ~: run isInQueryRenaming
        -- , "distinct on multiple queries" ~: run selectDistinct
        -- , "distinct on single query" ~: run selectValuesDistinct
        -- , "distinct restrict" ~: run selectRestrictedDistinct
        -- , "matchNull" ~: run simpleMatchNull
        -- , "ifThenElse" ~: run simpleIfThenElse
        -- , -- , "validateTable validates"            ~: run validateTableValidates
        --   "aggregate empty table" ~: run aggregateEmptyTable
        -- , "empty singleton values" ~: run selectValuesEmptySingletonTable
        -- , "coalesce row" ~: run coalesceRow
        -- , "coalesce equality" ~: run coalesceEquality
        -- , "coalesce num" ~: run coalesceNum
        -- , "coalesce frac" ~: run coalesceFrac
        -- , "coalesce sum" ~: run coalesceSum
        -- , "nonNull" ~: run nonNullYieldsEmptyResult
        -- , "rawQuery1" ~: run rawQuery1Works
        -- , "rawQuery" ~: run rawQueryWorks
        -- , -- , "union"                              ~: run unionWorks
        --   "union discards dupes" ~: run unionDiscardsDupes
        -- , "union works for whole rows" ~: run unionWorksForWholeRows
        -- , "expression cols under union (LHS)" ~: run unionWithLhsExpressionCols
        -- , "expression cols under union (RHS)" ~: run unionWithRhsExpressionCols
        -- , "unionAll" ~: run unionAllWorks
        -- , "unionAll works for whole rows" ~: run unionAllForWholeRows
        -- , "string concatenation" ~: run stringConcatenation
        -- , "teardown succeeds" ~: run teardown
        -- , "if not exists works" ~: run (setup >> resetup)
        ]

simpleSelect = do
    ppl <- query $ select people
    assEq "wrong results from select" (sort peopleItems) (sort ppl)

simpleProduct = do
    prod <- query $ do
        a <- select addresses
        p <- select people
        pure $ a.name :*: a.city :*: p
    assEq "wrong results from product" (sort ans) (sort prod)
  where
    ans = [a.name :*: a.city :*: p | p <- peopleItems, a <- addressItems]

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
        return (name :*: a.city)
    assEq "join-like query gave wrong result" (sort ans) (sort res)
  where
    ans =
        [ p.name :*: c
        | p <- peopleItems
        , Rec (_ := n', _ := c) <- addressItems
        , p.name == n'
        ]

joinLikeProductWithSels = do
    res <- query $ do
        p <- select people
        a <- select addresses
        restrict (p.name .== a.name)
        return (p.name :*: a.city :*: p.pet)
    assEq "join-like query gave wrong result" (sort ans) (sort res)
  where
    ans =
        [ n :*: c :*: p
        | Person n _ p _ <- map toNative peopleItems
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
        return (name :*: a.city)
    assEq "join-like query gave wrong result" (sort ans) (sort res)
  where
    ans =
        [ "Link" :*: Just "Kakariko"
        , "Velvet" :*: Nothing
        , "Miyu" :*: Just "Fuyukishi"
        , "Kobayashi" :*: Just "Tokyo"
        ]

rowLeftJoin = do
    res <- query $ do
        name <- (.name) <$> select people
        a <-
            leftJoin
                (\a -> name .== a.name)
                (select addresses)
        return (name :*: a)
    assEq "join-like query gave wrong result" (sort ans) (sort res)
  where
    ans =
        [ "Link" :*: Just (Rec (#name := "Link", #city := "Kakariko"))
        , "Velvet" :*: Nothing
        , "Miyu" :*: Just (Rec (#name := "Miyu", #city := "Fuyukishi"))
        , "Kobayashi" :*: Just (Rec (#name := "Kobayashi", #city := "Tokyo"))
        ]

leftJoinThenProduct = do
    res <- query $ do
        name <- (.name) <$> select people
        a <-
            leftJoin
                (\a -> name .== a.name)
                (select addresses)
        c <- select comments
        restrict $ c.name .== just name
        return $ name :*: a.city :*: c.comment
    assEq "join + product gave wrong result" ans res
  where
    linkComment = head [c | Rec (_ := n, _ := c) <- commentItems, n == Just "Link"]
    ans = ["Link" :*: Just "Kakariko" :*: linkComment]

countAggregate = do
    [res] <- query . aggregate $ do
        p <- select people
        return $ count p.pet
    assEq "count counted the wrong number of pets" ans res
  where
    ans = length [p | Just p <- map (.pet) peopleItems]

joinGroupAggregate = do
    res <- query . aggregate $ do
        p <- select people
        a <-
            leftJoin
                (\a -> p ! pName .== a ! aName)
                (select addresses)
        nopet <- groupBy (isNull (p ! pPet))
        return (nopet :*: count (a ?aCity))
    assEq "wrong number of cities per pet owneship status" ans (sort res)
  where
    -- There are pet owners in Tokyo and Kakariko, there is no pet owner in
    -- Fuyukishi
    ans = [False :*: 2, True :*: 1]

nestedLeftJoin = do
    res <- query $ do
        p <- select people
        _ :*: city :*: cs <- leftJoin (\(name' :*: _) -> p ! pName .== name') $ do
            a <- select addresses
            _ :*: cs <- leftJoin (\(n :*: _) -> n .== just (a ! aName)) $
                aggregate $ do
                    c <- select comments
                    n <- groupBy (c ! cName)
                    return (n :*: count (c ! cComment))
            return (a ! aName :*: a ! aCity :*: cs)
        return (p ! pName :*: city :*: cs)
    ass ("user with comment not in result: " ++ show res) (link `elem` res)
    ass ("user without comment not in result: " ++ show res) (velvet `elem` res)
  where
    link = "Link" :*: Just "Kakariko" :*: Just (1 :: Int)
    velvet = "Velvet" :*: Nothing :*: Nothing

orderLimit = do
    res <- query $
        limit 1 2 $ do
            t <- select people
            order (t ! pCash) descending
            return (t ! pName)
    assEq "got wrong result" ["Link", "Velvet"] (sort res)

limitCorrectNumber = do
    res <- query $ do
        p1 <- limit 1 2 $ select people
        p2 <- limit 1 2 $ select people
        return p1
    assEq ("wrong number of results from limit") 4 (length res)

aggregateWithDoubles = do
    [Just res] <- query $
        aggregate $ do
            cash <- pCash `from` select people
            return (avg cash)
    assEq "got wrong result" ans res
  where
    ans = sum (map cash peopleItems) / fromIntegral (length peopleItems)

selectVals = do
    vals <- query $ values peopleItems
    assEq "wrong columns returned" (sort peopleItems) (sort vals)

selectEmptyValues = do
    res <- query $ do
        ppl <- select people
        vals <- values ([] :: [Rec '["foo" := Maybe Text]])
        cs <- select comments
        pure cs
    assEq "result set wasn't empty" [] res

aggregateEmptyValues = do
    [res] <- query $
        aggregate $ do
            ppl <- select people
            vals <- values ([] :: [(Int, Int)])
            t <- select comments
            return (count (t ! cId))
    assEq "wrong count for empty result set" 0 res

testInnerJoin = do
    res <- query $ do
        p <- select people
        a <- innerJoin (\a -> p ! pName .== a ! aName) $ do
            select addresses
        return (p ! pPet :*: a ! aCity)
    assEq "wrong result" oracle res
  where
    oracle =
        [ Just "horse" :*: "Kakariko"
        , Just "dragon" :*: "Tokyo"
        , Nothing :*: "Fuyukishi"
        ]

simpleIfThenElse = do
    ppl <- query $ do
        t <- select people
        let ageGroup =
                ifThenElse (t ! pAge .< 18) (text "Child") $
                    ifThenElse
                        (t ! pAge .>= 65)
                        (text "Elder")
                        (text "Adult")
        return (t ! pName :*: t ! pAge :*: ageGroup)
    assEq "wrong results from ifThenElse" (sort res) (sort ppl)
  where
    res =
        [ "Link" :*: 125 :*: "Elder"
        , "Velvet" :*: 19 :*: "Adult"
        , "Kobayashi" :*: 23 :*: "Adult"
        , "Miyu" :*: 10 :*: "Child"
        ]

roundToInt = do
    res <- query $ do
        val <- values [1.1, 1.5, 1.9]
        return $ round_ (the val)
    assEq "bad rounding" [1, 2, 2 :: Int] res

serializeDouble = do
    -- The "protocol" used by PostgreSQL is insane - better check that we speak
    -- it properly!
    res <- query $ do
        n <- values [123456789 :: Int]
        d <- values [123456789.3 :: Double]
        restrict (the d .> cast (the n))
        return (cast (the n) + float 1.123)
    assEq "wrong encoding" 1 (length res)
    assEq "wrong decoding" [123456790.123] res

testSuchThat = do
    res <- query $ do
        n1 <- (pName `from` select people) `suchThat` (.== "Link")
        n2 <- (pName `from` select people) `suchThat` (.== "Velvet")
        return (n1 :*: n2)
    assEq "got wrong result" ["Link" :*: "Velvet"] res

{-# NOINLINE allShortNames #-}
allShortNames :: NeldaM [Text]
allShortNames = prepared $ do
    p <- select people
    restrict (length_ (p ! pName) .<= 4)
    order (p ! pName) ascending
    return (p ! pName)

preparedNoArgs = do
    res1 <- allShortNames
    res2 <- allShortNames
    res3 <- allShortNames
    assEq "got wrong result" res res1
    ass "subsequent calls gave different results" (all (== res1) [res2, res3])
  where
    res = ["Link", "Miyu"]

{-# NOINLINE allNamesLike #-}
-- Extra restricts to force the presence of a few non-argument parameters.
allNamesLike :: Int -> Text -> NeldaM [Text]
allNamesLike = prepared $ \len s -> do
    p <- select people
    restrict (length_ (p ! pName) .> 0)
    restrict (p ! pName `like` s)
    restrict (length_ (p ! pName) .> 1)
    restrict (length_ (p ! pName) .<= len)
    restrict (length_ (p ! pName) .<= 100)
    restrict (length_ (p ! pName) .<= 200)
    order (p ! pName) ascending
    return (p ! pName)

preparedManyArgs = do
    res1 <- allNamesLike 4 "%y%"
    res2 <- allNamesLike 5 "%y%"
    res3 <- allNamesLike 6 "%y%"
    assEq "got wrong result" res res1
    ass "subsequent calls gave different results" (all (== res1) [res2, res3])
  where
    res = ["Miyu"]

preparedInterleaved = do
    asn1 <- allShortNames
    anl1 <- allNamesLike 4 "%y%"
    asn2 <- allShortNames
    anl2 <- allNamesLike 5 "%y%"
    asn3 <- allShortNames
    anl3 <- allNamesLike 6 "%y%"
    assEq "wrong result from allShortNames" asnres asn1
    assEq "wrong result from allNamesLike" anlres anl1
    ass
        "subsequent allShortNames calls gave different results"
        (all (== asn1) [asn2, asn3])
    ass
        "subsequent allNamesLike calls gave different results"
        (all (== anl1) [anl2, anl3])
  where
    asnres = ["Link", "Miyu"]
    anlres = ["Miyu"]

preparedDifferentResults = do
    res1 <- allNamesLike 4 "%y%"
    res2 <- allNamesLike 10 "%y%"
    assEq "wrong result from first query" ["Miyu"] res1
    assEq "wrong result from second query" ["Kobayashi", "Miyu"] res2

orderCorrectOrder = do
    insert_ people [Person "Amber" 19 Nothing 123]

    res1 <- query $ do
        p <- select people
        order (p ! pName) ascending
        order (p ! pAge) ascending
        return (p ! pName)

    res2 <- query $ do
        p <- select people
        order (p ! pName) descending
        order (p ! pAge) ascending
        return (p ! pName)

    res3 <- query $ do
        p <- select people
        order (p ! pAge) descending
        order (p ! pName) ascending
        return (p ! pName)

    deleteFrom_ people $ \p -> p ! pName .== "Amber"

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

        (owner :*: homes) <- aggregate $ do
            a <- select addresses
            owner' <- groupBy (a ! aName)
            return (owner' :*: count (a ! aCity))
        restrict (owner .== p ! pName)

        (owner2 :*: homesInTokyo) <- aggregate $ do
            a <- select addresses
            restrict (a ! aCity .== "Tokyo")
            owner' <- groupBy (a ! aName)
            return (owner' :*: count (a ! aCity))
        restrict (owner2 .== p ! pName)

        order homes descending
        return (owner :*: homes :*: homesInTokyo)
    assEq "wrong result for aggregate query" ["Kobayashi" :*: 1 :*: 1] res

isInQueryRenaming = do
    res <- query $ do
        t1 <- select people
        restrict $
            (int 1)
                `isIn` ( do
                            t2 <- select people
                            t3 <- select addresses
                            restrict (t3 ! aName .== t2 ! pName)
                            restrict (t1 ! pName .== t2 ! pName)
                            restrict (t3 ! aCity .== "Kakariko")
                            return (int 1)
                       )
        return (t1 ! pName)
    assEq "wrong list of people returned" ["Link"] res

selectDistinct = do
    res <- query $
        distinct $ do
            t <- select people
            select people
            order (t ! pName) ascending
            return (t ! pName)
    assEq "wrong result set" ["Kobayashi", "Link", "Miyu", "Velvet"] res

data L = L Text
    deriving (Generic, Show, Eq)

-- instance SqlRow L

selectValuesDistinct = do
    res <- query $ distinct $ selectValues $ replicate 5 (L "Link")
    assEq "wrong result set" [L "Link"] res

data Distinct = D {a :: Int, b :: Int}
    deriving (Generic)

-- instance SqlRow Distinct

selectRestrictedDistinct = do
    xs <- query $
        distinct $ do
            x <- selectValues [D 42 1, D 42 2, D 42 3]
            restrict $ x ! d_b .>= 2
            return $ x ! d_a
    assEq "wrong result set" [42] xs

simpleMatchNull = do
    res <- query $ do
        t <- select people
        order (t ! pName) ascending
        return $ (t ! pName :*: matchNull 0 length_ (t ! pPet))
    assEq "wrong result set" expected res
  where
    expected =
        [ "Kobayashi" :*: 6
        , "Link" :*: 5
        , "Miyu" :*: 0
        , "Velvet" :*: 0
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
        aggregate $ count <$> s_fst `from` selectValues ([] :: [(Int, Int)])
    assEq "count of empty table not 0" (0 :: Int) res

    [res] <- query $ do
        aggregate $ sum_ <$> s_fst `from` selectValues ([] :: [(Int, Int)])
    (res :: Int) `seq` return ()
    assEq "sum of empty table not 0" (0 :: Int) res

    [res] <- query $ do
        aggregate $ avg <$> s_fst `from` selectValues ([] :: [(Int, Int)])
    assEq "average of empty table not Nothing" (Nothing :: Maybe Int) res

    [res] <- query $ do
        aggregate $ min_ <$> s_fst `from` selectValues ([] :: [(Int, Int)])
    assEq "min of empty table not Nothing" (Nothing :: Maybe Int) res

    [res] <- query $ do
        aggregate $ max_ <$> s_fst `from` selectValues ([] :: [(Int, Int)])
    assEq "max of empty table not Nothing" (Nothing :: Maybe Int) res
  where
    s_fst = undefined -- unsafeSelector 0 :: Selector (Int, Int) Int

selectValuesEmptySingletonTable = do
    [res] <- query $ aggregate $ count <$> the <$> selectValues ([] :: [Int])
    assEq "non-zero count when selecting from empty values" 0 res

coalesceRow = do
    empty <- query $ do
        _ <- selectValues [Only (1 :: Int)]
        xs <- leftJoin (const false) (select people)
        return (xs ?! pName)
    assEq "result not single null" [Nothing] empty

    res <- query $ do
        person <- select people
        restrict' (person ?! pName ?== text "Link")
        xs <- leftJoin (const true) (select people)
        order (xs ?! pName) ascending
        return $ xs ?! pName
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
        restrict' (person ! pPet ?== text "horse")
        return (person ! pName)
    ["Kobayashi"] <- query $ do
        person <- select people
        restrict' (person ! pPet ?/= text "horse")
        return (person ! pName)
    return ()

coalesceNum :: NeldaM ()
coalesceNum = do
    [Just 250 :*: Just 126 :*: Just 124] <- query $ do
        _ <- selectValues [Only (1 :: Int)]
        person <- leftJoin (const true) (select people)
        restrict' (person ?! pName ?== text "Link")
        return
            ( person ?! pAge ?* int 2
                :*: person ?! pAge ?+ int 1
                :*: person ?! pAge ?- int 1
            )
    return ()

coalesceFrac = do
    result <- query $ do
        _ <- selectValues [Only (1 :: Int)]
        person <- leftJoin (const true) (select people)
        restrict' (person ?! pName ?== text "Miyu")
        return
            ( person ?! pCash ?/ float 2
                :*: person ?! pAge ?/ int 2
            )
    assEq "wrong calculation results" [Just (-250) :*: Just 5] result

coalesceSum = do
    res <- query $
        aggregate $ do
            _ <- selectValues [Only (1 :: Int)]
            person <- leftJoin (const true) (select people)
            age <- nonNull $ person ?! pAge
            return $ sum_ age
    assEq "wrong sum" [125 + 19 + 23 + 10 :: Int] res

nonNullYieldsEmptyResult = do
    empty <- query $ do
        _ <- selectValues [Only (1 :: Int)]
        person <- leftJoin (const false) (select people)
        nonNull $ person ?! pAge
    assEq "empty list not empty" [] empty

rawQuery1Works = do
    names <- query $ do
        n <- rawQuery1 "name" "SELECT name FROM people"
        order n ascending
        return n
    assEq "wrong name list returned" (sort $ map name peopleItems) names

rawQueryWorks = do
    ppl <- query $ do
        p <-
            rawQuery
                ["name", "age", "pet", "cash"]
                ("SELECT * FROM people WHERE name = " <> injLit ("Link" :: Text))
        return p
    let correct = [p | p <- peopleItems, name p == "Link"]
    assEq "wrong name list returned" correct ppl

unionworks = assqueryeq "wrong name list returned" correct $ do
    let ppl = pname `from` select people
        pets = (ppet `from` select people) >>= nonnull
    name <- nelda . toupper <$> union pets ppl
    order name ascending
    return name
  where
    correct =
        sort $
            map (text . map char . toupper) $
                map name peopleitems ++ catmaybes (map pet peopleitems)

unionDiscardsDupes = assQueryEq "wrong name list returned" correct $ do
    let ppl = pName `from` select people
        pets = (pPet `from` select people) >>= nonNull
    name <- Nelda.toUpper <$> ppl `union` pets `union` ppl `union` ppl
    order name ascending
    return name
  where
    correct =
        sort $
            map (Text.map Char.toUpper) $
                map name peopleItems
                    ++ catMaybes (map pet peopleItems)

unionWorksForWholeRows = assQueryEq "wrong person list returned" correct $ do
    let ppl1 = select people `suchThat` \p -> p ! pAge .<= 18
        ppl2 = select people `suchThat` \p -> p ! pAge .> 18
    ppl <- ppl1 `union` ppl2 `union` ppl2 `union` ppl1
    order (Nelda.toUpper (ppl ! pName)) ascending
    return ppl
  where
    correct = sortBy (compare `on` (Text.map Char.toUpper . name)) peopleItems

unionWithLhsExpressionCols = assQueryEq "wrong person list returned" correct $ do
    let ppl1 = select people
        ppl2 = (`with` [pAge += 1]) <$> select people
    ppl <- ppl2 `union` ppl1
    order (ppl ! pAge) ascending
    return ppl
  where
    correct = sortBy (compare `on` age) (peopleItems ++ map (\p -> p{age = age p + 1}) peopleItems)

unionWithRhsExpressionCols = assQueryEq "wrong person list returned" correct $ do
    let ppl1 = select people
        ppl2 = (`with` [pAge += 1]) <$> select people
    ppl <- ppl1 `union` ppl2
    order (ppl ! pAge) ascending
    return ppl
  where
    correct = sortBy (compare `on` age) (peopleItems ++ map (\p -> p{age = age p + 1}) peopleItems)

unionAllWorks = assQueryEq "wrong name list returned" correct $ do
    let ppl = pName `from` select people
        pets = (pPet `from` select people) >>= nonNull
    name <- Nelda.toUpper <$> ppl `unionAll` pets `unionAll` ppl
    order name ascending
    return name
  where
    correct =
        sort $
            map (Text.map Char.toUpper) $
                map name peopleItems
                    ++ map name peopleItems
                    ++ catMaybes (map pet peopleItems)

unionAllForWholeRows = assQueryEq "wrong person list returned" correct $ do
    let ppl1 = select people
        ppl2 = (`with` [pAge += 1]) <$> select people
    ppl <- ppl1 `unionAll` ppl2
    order (ppl ! pAge) ascending
    return ppl
  where
    correct = sortBy (compare `on` age) (peopleItems ++ map (\p -> p{age = age p + 1}) peopleItems)

stringConcatenation = assQueryEq "wrong string returned" ["abcde"] $ do
    pure $ mconcat ["a" :: Col s Text, "bc", "", "de"]

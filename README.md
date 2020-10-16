🚧 Experiment fork of Selda
===========================
**WARN: WIP**

[Selda](https://github.com/valderman/selda) is a Haskell library for interacting with SQL-based relational databases.
Compared to other haskell type-safe SQL libraries, for example Beam and Opaleye, `Selda` feels more lightwieght and simple. But to keep the library ease of use, there is some issues.


This fork aims to fix those issues while trying to keeping the simplicity of `Selda` as possible.
The major differences are:

* Use GHC's [Backpack](https://gitlab.haskell.org/ghc/ghc/-/wikis/backpack) to implement per-DB features
* Use Extensible Record for table row input and output. Currently using [jrec](https://github.com/juspay/jrec).
* Value-level table schema definition insted of using record data type.

How it looks like
=================

```haskell
data Pet = Dog | Horse | Dragon
    deriving (Show, Read, Bounded, Enum)
    deriving SqlType via SqlTypeDeriving.TextEnum Pet

people :: _
people = table #people
    ( column #name T.text & notNull
    , column #age  T.int & notNull
    , column #pet  (T.text & asSqlType @Pet)
    )

test :: IO _
test = withSQLite "people.sqlite" $ do
    insert_ people
        [ Rec (#name := "Velvet", #age := 19, #pet := Just Dog)
        , Rec (#name := "Kobayashi", #age := 23, #pet := Just Dragon)
        , Rec (#name := "Miyu", #age := 10, #pet := Nothing)
        ]

    query $ do
        row <- select people
        restrict $ row.age .>= 18
        pure row
```

Motivation
==========

🚧

Things TODO
=============================

* [x] ENUM support(TEXT type backend)
* [ ] Add more column-level constraints/attirbutes
* [ ] Table creation
* [ ] Table validation
* [ ] Table migration
* [ ] Table-level constraints/attributes
* [ ] Add more columns type
* [ ] JSON support
* [ ] ENUM support(PostgreSQL)
* [ ] Tests

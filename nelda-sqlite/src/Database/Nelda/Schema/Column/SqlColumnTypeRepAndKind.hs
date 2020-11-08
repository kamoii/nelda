module Database.Nelda.Schema.Column.SqlColumnTypeRepAndKind where

-- DBが提供する型
-- ただ INT と INT UNSINGED は別にしないといけない(MySQL上では単一の型だけど以下の理由で分ける必要がある)
-- 対応する haskell type が一意に決まる必要がある。
-- haskell type は単独で backend library の parameter や 結果から一意的に変換できる必要あり。
-- つまり Posgres だと oid に対応している必要あり
--
-- 以下の gadts 的にどうなんだ？？？
data SqlColumnTypeRep
    = RInt
    | RText
    | RDouble
    | RBoolean
    deriving (Show)

{-
data SqlColumnTypeRep where
    TUnsingedInt :: Maybe Int -> SqlColumnTypeRep
    TInt :: SqlColumnTypeRep
    TText :: SqlColumnTypeRep
    TSerial :: SqlColumnTypeRep
-}

data SqlColumnTypeKind
    = TInt
    | TText
    | TDouble
    | TBoolean

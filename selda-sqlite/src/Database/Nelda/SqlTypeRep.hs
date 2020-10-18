module Database.Nelda.SqlTypeRep where

-- * SqlTypeRep
-- SqlType 型クラスと一対一にすべき

-- | Representation of an SQL type.
-- 通常 TText, TInt, TDateTime のような形式
-- TRowID は SQL backend の型ではなく, selda の都合上導入されていた論理型なので不要
-- `BOOLEAN' などの別名はサポートするべきか？
data SqlTypeRep
    = TInteger
    | TFloat
    | TText
    | TBlob
    | TRowID   -- DEPRECATE予定
    | TBoolean
    deriving (Show, Eq, Ord)

rowIDSqlType :: SqlTypeRep
rowIDSqlType = TRowID

isCompatibleWith :: SqlTypeRep -> SqlTypeRep -> Bool
isCompatibleWith TRowID TInteger = True
isCompatibleWith TInteger TRowID = True
isCompatibleWith a b             = a == b

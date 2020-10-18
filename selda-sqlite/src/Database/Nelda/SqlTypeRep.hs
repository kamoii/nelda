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
    | TBoolean
    deriving (Show, Eq, Ord)

isCompatibleWith :: SqlTypeRep -> SqlTypeRep -> Bool
isCompatibleWith a b             = a == b

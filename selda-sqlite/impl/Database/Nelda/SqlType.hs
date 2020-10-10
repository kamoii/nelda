{-# LANGUAGE TypeFamilies #-}

module Database.Nelda.SqlType where

import Data.Text (Text)

-- * SqlTypeRep

data SqlTypeRep
    = STInt
    | STText
    deriving (Show, Eq, Ord)

-- * SqlType

class Show st => SqlType st where
    type OriginSqlType st
    -- type OriginSqlType st = st

    -- embed :: st -> SqlFragment

instance SqlType Int where
    type OriginSqlType Int = Int

instance SqlType Text where
    type OriginSqlType Text = Text


-- instance SqlType Word where
--     type OriginSqlType Word = Word

-- instance SqlType Text where
--     type OriginSqlType Text = Text

-- instance SqlType Int where
--     type OriginSqlType Int = Int

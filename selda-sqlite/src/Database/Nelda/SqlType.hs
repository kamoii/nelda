{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.SqlType
    ( module Database.Nelda.Backend.Types
    , module Database.Nelda.SqlTypeClass
    , module Database.Nelda.SqlTypeRep
    ) where

import Database.Nelda.Backend.Types
import Database.Nelda.SqlTypeClass
import Database.Nelda.SqlTypeRep
import Data.Text as Text (pack, replace)
import Data.Text (Text)
import Database.SQLite3

instance SqlType Int where
    type OriginSqlType Int = Int
    sqlTypeRep = TInteger
    toSqlParam i = SQLInteger $ fromIntegral i -- TODO: いいのか？
    fromSqlValue (SQLInteger i) = fromIntegral i  -- TODO: いいのか？
    toSqlExpression i = Text.pack $ show i

instance SqlType Text where
    type OriginSqlType Text = Text
    sqlTypeRep = TText
    toSqlParam t = SQLText t
    fromSqlValue (SQLText t) = t
    -- https://sqlite.org/lang_expr.html
    -- 3. Literal Values (Constants)
    --
    -- A string constant is formed by enclosing the string in single quotes (').
    -- A single quote within the string can be encoded by putting two single quotes in a row - as in Pascal.
    -- C-style escapes using the backslash character are not supported because they are not standard SQL.
    toSqlExpression t = "'" <> Text.replace "'" "''" t <> "'"

instance SqlType Double where
    type OriginSqlType Double = Double
    sqlTypeRep = TFloat
    toSqlParam d = SQLFloat d
    fromSqlValue (SQLFloat d) = d
    toSqlExpression _d = error "NOT IMPLEMENTED YET"

instance SqlType Bool where
    type OriginSqlType Bool = Bool
    sqlTypeRep = TBoolean
    toSqlParam b = SQLInteger $ if b then 1 else 0
    fromSqlValue (SQLInteger i) = not (i==0)
    toSqlExpression _d = error "NOT IMPLEMENTED YET"

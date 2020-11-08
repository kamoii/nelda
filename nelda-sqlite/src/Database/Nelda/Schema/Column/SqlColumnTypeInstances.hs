{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Database.Nelda.Schema.Column.SqlColumnTypeInstances where

import Database.Nelda.Schema.Column.SqlColumnTypeRepAndKind
import Database.Nelda.Schema.Column.SqlColumnTypeClass
import Data.Text (Text)


instance SqlColumnType 'TInt where
    type ToSqlType 'TInt = Int

instance SqlColumnType 'TText where
    type ToSqlType 'TText = Text

instance SqlColumnType 'TDouble where
    type ToSqlType 'TDouble = Double

instance SqlColumnType 'TBoolean where
    type ToSqlType 'TBoolean = Bool

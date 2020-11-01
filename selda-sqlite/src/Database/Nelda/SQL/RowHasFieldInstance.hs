{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Nelda.SQL.RowHasFieldInstance where

import Database.Nelda.SQL.Selector
import Database.Nelda.SQL.Row (Row)
import Database.Nelda.SQL.Col (Col)
import GHC.OverloadedLabels (IsLabel(fromLabel))
import Database.Nelda.SqlTypeClass (NullableSqlType)

-- GHC が RecordDotSyntax をサポートするまでは
-- https://github.com/ndmitchell/record-dot-preprocessor

instance
    ( HasOrderedField name t a
    , NullableSqlType a aIsNullable
    , '(coln, a') ~ ColFromRow rown a aIsNullable
    ) => GHC.Records.Extra.HasField name (Row s rown t) (Col s coln a') where
    hasField row =
        ( error "set/modify is not supported for Row"
        , row ! fromLabel @name @(Selector t a)
        )

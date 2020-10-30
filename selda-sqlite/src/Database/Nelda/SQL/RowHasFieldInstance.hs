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
import Database.Nelda.SqlType (SqlType)
import Database.Nelda.SQL.Row (Row)
import Database.Nelda.SQL.Col (Col)
import GHC.OverloadedLabels (IsLabel(fromLabel))
import Data.CoalesceMaybe (CoalesceMaybe)

-- GHC が RecordDotSyntax をサポートするまでは
-- https://github.com/ndmitchell/record-dot-preprocessor

instance {-# OVERLAPPABLE #-}
    ( HasOrderedField name t a
    , a' ~ a
    , SqlType a'
    ) => GHC.Records.Extra.HasField name (Row s t) (Col s a') where
    hasField row =
        ( error "set/modify is not supported for Row"
        , row ! fromLabel @name @(Selector t a)
        )

instance
    ( HasOrderedField name t a
    , a' ~ CoalesceMaybe (Maybe a)
    , SqlType a
    ) => GHC.Records.Extra.HasField name (Row s (Maybe t)) (Col s a') where
    hasField row =
        ( error "set/modify is not supported for Row"
        , row ? fromLabel @name @(Selector t a)
        )

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Nelda.SQL.RowHasFieldInstance where

import Database.Nelda.SQL.Col (Col)
import Database.Nelda.SQL.Nullability
import Database.Nelda.SQL.Row (C, Row)
import Database.Nelda.SQL.Selector
import Database.Nelda.SqlType (SqlType)
import GHC.OverloadedLabels (IsLabel (fromLabel))
import qualified GHC.Records.Compat

-- GHC が RecordDotSyntax をサポートするまでは
-- https://github.com/ndmitchell/record-dot-preprocessor
--
-- (!=) が NonNull な Row にしか定義できないため set は なし。
-- HasField が read-only field をサポートするなら NonNull と Nullable で insatnce を分けるれのだが...

instance
    ( HasOrderedField name t v
    , v ~ C n1 a
    , a' ~ a
    , SqlType a
    , n2 ~ (n0 :.: n1)
    ) =>
    GHC.Records.Compat.HasField name (Row s n0 t) (Col s n2 a')
    where
    hasField row =
        ( error "set/modify is not supported for Row"
        , row ! fromLabel @name @(Selector t n1 a)
        )

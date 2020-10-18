{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Nelda.SqlTypeClass where

import Database.Nelda.SqlTypeRep
import Database.Nelda.Backend.Types
import Data.Text (Text)
import Data.Typeable (Typeable)

-- * SqlType

-- TODO: 何を SqlType にするべきか明確にする

-- ベースのもの(ライブラリが実装するもの)は st ~ OriginSqlType st となる。
-- 自分で拡張する際は OriginSqlType st と,メソッド間の整合が取れている必要がある
-- TODO: 安全に拡張してもらうために Deriving Strategy を提供すればいいかな？
-- TODO: Show 制約は Table や Column 関連の Show 導出を可能にするため
--       美しくはないが,基本 SqlType で Show じゃないものはないはずなので。
-- TODO: Typeable 制約は SqlType のために付けているだけで後から外せるかも
class (Typeable st, Show st) => SqlType st where
    -- 本来なら SqlType (OriginSqlType st) という制約を付けたいが, GHC は superclass loopは許していない。
    -- OriginSqlType の用途は カラム定義で デフォルトの SqlType 以外を利用する場合に,
    -- 変えた型がデフォルトの SqlType と互換性があるかのチェックである。
    -- その際の制約が ToSqlType ct ~ OriginSqlType st' であり, SqlType (ToSqlType ct) なので
    -- 無理にここで制約をかける必要はない。
    type OriginSqlType st
    -- .hsig ではデフォルト実装は許されていない
    -- type OriginSqlType st = st

    -- カラム定義の DEFAULT値の埋込みを考えている。
    -- が,パラメータで渡せるなら不要かな？
    -- PREPARED STAATMENT時に 静的に定まるパラメータを埋込むことで最適化を図れる可能性はあるかな。
    -- embed :: st -> SqlFragment

    -- | The SQL representation for this type.
    sqlTypeRep :: SqlTypeRep
    -- | Create a literal of this type.
    toSqlParam :: st -> SqlParam
    -- | Convert an SqlValue into this type.
    fromSqlValue :: SqlValue -> st
    -- | When embeding directly in SQL (e.g. DEFAULT caouse)
    toSqlExpression :: st -> Text

-- Maybe instance は特別扱い
-- Maybe a はベースタイプではない。
-- NULLable を Maybe で表現するので
-- ただ Maybe (Maybe Int) みたいな意味のない型も許してしまう,が Maybe が SqlType ではない
-- のは面倒なので。
-- 実装は backend に限らず同じになるが... いや, NULL値の扱いが異なるはずなので違うのか？..
instance SqlType a => SqlType (Maybe a) where
    -- TODO: そもそも あれか... OriginSqlType の用途的に TypeError でいいきがする
    type OriginSqlType (Maybe a) = Maybe (OriginSqlType a)
    sqlTypeRep = sqlTypeRep @a
    toSqlParam Nothing  = nullSqlParam
    toSqlParam (Just a) = toSqlParam a
    fromSqlValue v
        | isSqlValueNull v = Nothing
        | otherwise = Just $ fromSqlValue v
    toSqlExpression Nothing  = "NULL"
    toSqlExpression (Just a) = toSqlExpression a

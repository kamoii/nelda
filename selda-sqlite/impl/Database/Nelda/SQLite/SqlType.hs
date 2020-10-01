{-# LANGUAGE TypeFamilies #-}

module Database.Nelda.SQLite.SqlType where

-- * SqlTypeRep
-- SqlType 型クラスと一対一にすべき

data SqlTypeRep

instance Show SqlTypeRep
instance Eq SqlTypeRep
instance Ord SqlTypeRep

-- * SqlType

-- TODO: 何を SqlType にするべきか明確にする

-- ベースのもの(ライブラリが実装するもの)は st ~ OriginSqlType st となる。
-- 自分で拡張する際は OriginSqlType st と,メソッド間の整合が取れている必要がある
-- (安全に拡張してもらうために Deriving Strategy を提供すればいいかな？)
class SqlType st where
    type OriginSqlType st

    -- カラム定義の DEFAULT値の埋込みを考えている。
    -- が,パラメータで渡せるなら不要かな？
    -- PREPARED STAATMENT時に 静的に定まるパラメータを埋込むことで最適化を図れる可能性はあるかな。
    -- embed :: st -> SqlFragment

-- instance SqlType Word where
--     type OriginSqlType Word = Word

-- instance SqlType Text where
--     type OriginSqlType Text = Text

-- instance SqlType Int where
--     type OriginSqlType Int = Int

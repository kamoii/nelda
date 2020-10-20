{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Nelda.Schema.IsColumns where

import Database.Nelda.Schema.Column

-- 基本的に hetero-list 作っているってことか。
-- 型情報 Column のものを保持したまま Untyped Column を内部に持つという話なので
-- TODO: 一定の長さ(32?)までは tuple を用意して,後は任意の長さでも対応できるように

-- この構造は IsColNames と同じだが, 1要素の場合に型族で詰むのと
-- 型エラーが不味いので別実装で
-- エラーメッセージをマシにするために ~ を使っている...
class IsColumns columns where
    type ToColumnsType columns :: [*]
    toAnyColumns :: columns -> [AnyColumn]

-- TODO: warn を suppress したい
-- instance {-# OVERLAPPABLE #-} (TypeError (TL.Text "foo")) => IsColumns v

-- TODO: 0個(()の instance)のカラムテーブルを許容するか？
-- ただ...このレベルでチェックではない気がする。table 関数がチェックするべきかな..
-- あくまで カラム定義の集合なので

-- ここだけは ~ が使えない(type ToColumnsType が他のものと衝突する(conflicting type family instances)
-- なので上記のエラーメッセージほいほいを使う
instance IsColumns (Column a0 b0 c0 d0 e0 f0) where
    type ToColumnsType (Column a0 b0 c0 d0 e0 f0) = '[Column a0 b0 c0 d0 e0 f0]
    toAnyColumns (v0) = [AnyColumn v0]

instance (v0 ~ Column a0 b0 c0 d0 e0 f0, v1 ~ Column a1 b1 c1 d1 e1 f1) => IsColumns (v0,v1) where
    type ToColumnsType (v0, v1) = '[v0, v1]
    toAnyColumns (v0, v1) = [AnyColumn v0, AnyColumn v1]

instance (v0 ~ Column a0 b0 c0 d0 e0 f0, v1 ~ Column a1 b1 c1 d1 e1 f1, v2 ~ Column a2 b2 c2 d2 e2 f2) => IsColumns (v0,v1,v2) where
    type ToColumnsType (v0, v1, v2) = '[v0, v1, v2]
    toAnyColumns (v0, v1, v2) = [AnyColumn v0, AnyColumn v1, AnyColumn v2]

columns
    :: IsColumns columns
    => columns
    -> Columns (ToColumnsType columns)
columns = Columns . toAnyColumns

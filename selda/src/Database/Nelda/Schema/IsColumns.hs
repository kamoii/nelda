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

{-
def n_tuple_instance(n)
  constraint = n.times.map{ |i| "v#{i} ~ Column a#{i} b#{i} c#{i} d#{i} e#{i} f#{i}" }.join(',')
  vars = n.times.map{ |i| "v#{i}" }.join(',')
  vars_any = n.times.map{ |i| "AnyColumn v#{i}" }.join(',')
  <<-EOS
instance (#{constraint}) => IsColumns (#{vars}) where
    type ToColumnsType (#{vars}) = '[#{vars}]
    toAnyColumns (#{vars}) = [#{vars_any}]
  EOS
end

2.upto(16) do |n|
  puts n_tuple_instance(n)
end
-}

-- ここだけは ~ が使えない(type ToColumnsType が他のものと衝突する(conflicting type family instances)
-- なので上記のエラーメッセージほいほいを使う
instance IsColumns (Column a0 b0 c0 d0 e0 f0) where
    type ToColumnsType (Column a0 b0 c0 d0 e0 f0) = '[Column a0 b0 c0 d0 e0 f0]
    toAnyColumns (v0) = [AnyColumn v0]

instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1) => IsColumns (v0,v1) where
    type ToColumnsType (v0,v1) = '[v0,v1]
    toAnyColumns (v0,v1) = [AnyColumn v0,AnyColumn v1]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2) => IsColumns (v0,v1,v2) where
    type ToColumnsType (v0,v1,v2) = '[v0,v1,v2]
    toAnyColumns (v0,v1,v2) = [AnyColumn v0,AnyColumn v1,AnyColumn v2]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3) => IsColumns (v0,v1,v2,v3) where
    type ToColumnsType (v0,v1,v2,v3) = '[v0,v1,v2,v3]
    toAnyColumns (v0,v1,v2,v3) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3,v4 ~ Column a4 b4 c4 d4 e4 f4) => IsColumns (v0,v1,v2,v3,v4) where
    type ToColumnsType (v0,v1,v2,v3,v4) = '[v0,v1,v2,v3,v4]
    toAnyColumns (v0,v1,v2,v3,v4) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3,AnyColumn v4]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3,v4 ~ Column a4 b4 c4 d4 e4 f4,v5 ~ Column a5 b5 c5 d5 e5 f5) => IsColumns (v0,v1,v2,v3,v4,v5) where
    type ToColumnsType (v0,v1,v2,v3,v4,v5) = '[v0,v1,v2,v3,v4,v5]
    toAnyColumns (v0,v1,v2,v3,v4,v5) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3,AnyColumn v4,AnyColumn v5]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3,v4 ~ Column a4 b4 c4 d4 e4 f4,v5 ~ Column a5 b5 c5 d5 e5 f5,v6 ~ Column a6 b6 c6 d6 e6 f6) => IsColumns (v0,v1,v2,v3,v4,v5,v6) where
    type ToColumnsType (v0,v1,v2,v3,v4,v5,v6) = '[v0,v1,v2,v3,v4,v5,v6]
    toAnyColumns (v0,v1,v2,v3,v4,v5,v6) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3,AnyColumn v4,AnyColumn v5,AnyColumn v6]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3,v4 ~ Column a4 b4 c4 d4 e4 f4,v5 ~ Column a5 b5 c5 d5 e5 f5,v6 ~ Column a6 b6 c6 d6 e6 f6,v7 ~ Column a7 b7 c7 d7 e7 f7) => IsColumns (v0,v1,v2,v3,v4,v5,v6,v7) where
    type ToColumnsType (v0,v1,v2,v3,v4,v5,v6,v7) = '[v0,v1,v2,v3,v4,v5,v6,v7]
    toAnyColumns (v0,v1,v2,v3,v4,v5,v6,v7) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3,AnyColumn v4,AnyColumn v5,AnyColumn v6,AnyColumn v7]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3,v4 ~ Column a4 b4 c4 d4 e4 f4,v5 ~ Column a5 b5 c5 d5 e5 f5,v6 ~ Column a6 b6 c6 d6 e6 f6,v7 ~ Column a7 b7 c7 d7 e7 f7,v8 ~ Column a8 b8 c8 d8 e8 f8) => IsColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8) where
    type ToColumnsType (v0,v1,v2,v3,v4,v5,v6,v7,v8) = '[v0,v1,v2,v3,v4,v5,v6,v7,v8]
    toAnyColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3,AnyColumn v4,AnyColumn v5,AnyColumn v6,AnyColumn v7,AnyColumn v8]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3,v4 ~ Column a4 b4 c4 d4 e4 f4,v5 ~ Column a5 b5 c5 d5 e5 f5,v6 ~ Column a6 b6 c6 d6 e6 f6,v7 ~ Column a7 b7 c7 d7 e7 f7,v8 ~ Column a8 b8 c8 d8 e8 f8,v9 ~ Column a9 b9 c9 d9 e9 f9) => IsColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9) where
    type ToColumnsType (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9) = '[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9]
    toAnyColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3,AnyColumn v4,AnyColumn v5,AnyColumn v6,AnyColumn v7,AnyColumn v8,AnyColumn v9]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3,v4 ~ Column a4 b4 c4 d4 e4 f4,v5 ~ Column a5 b5 c5 d5 e5 f5,v6 ~ Column a6 b6 c6 d6 e6 f6,v7 ~ Column a7 b7 c7 d7 e7 f7,v8 ~ Column a8 b8 c8 d8 e8 f8,v9 ~ Column a9 b9 c9 d9 e9 f9,v10 ~ Column a10 b10 c10 d10 e10 f10) => IsColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10) where
    type ToColumnsType (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10) = '[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10]
    toAnyColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3,AnyColumn v4,AnyColumn v5,AnyColumn v6,AnyColumn v7,AnyColumn v8,AnyColumn v9,AnyColumn v10]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3,v4 ~ Column a4 b4 c4 d4 e4 f4,v5 ~ Column a5 b5 c5 d5 e5 f5,v6 ~ Column a6 b6 c6 d6 e6 f6,v7 ~ Column a7 b7 c7 d7 e7 f7,v8 ~ Column a8 b8 c8 d8 e8 f8,v9 ~ Column a9 b9 c9 d9 e9 f9,v10 ~ Column a10 b10 c10 d10 e10 f10,v11 ~ Column a11 b11 c11 d11 e11 f11) => IsColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11) where
    type ToColumnsType (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11) = '[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11]
    toAnyColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3,AnyColumn v4,AnyColumn v5,AnyColumn v6,AnyColumn v7,AnyColumn v8,AnyColumn v9,AnyColumn v10,AnyColumn v11]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3,v4 ~ Column a4 b4 c4 d4 e4 f4,v5 ~ Column a5 b5 c5 d5 e5 f5,v6 ~ Column a6 b6 c6 d6 e6 f6,v7 ~ Column a7 b7 c7 d7 e7 f7,v8 ~ Column a8 b8 c8 d8 e8 f8,v9 ~ Column a9 b9 c9 d9 e9 f9,v10 ~ Column a10 b10 c10 d10 e10 f10,v11 ~ Column a11 b11 c11 d11 e11 f11,v12 ~ Column a12 b12 c12 d12 e12 f12) => IsColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12) where
    type ToColumnsType (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12) = '[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12]
    toAnyColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3,AnyColumn v4,AnyColumn v5,AnyColumn v6,AnyColumn v7,AnyColumn v8,AnyColumn v9,AnyColumn v10,AnyColumn v11,AnyColumn v12]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3,v4 ~ Column a4 b4 c4 d4 e4 f4,v5 ~ Column a5 b5 c5 d5 e5 f5,v6 ~ Column a6 b6 c6 d6 e6 f6,v7 ~ Column a7 b7 c7 d7 e7 f7,v8 ~ Column a8 b8 c8 d8 e8 f8,v9 ~ Column a9 b9 c9 d9 e9 f9,v10 ~ Column a10 b10 c10 d10 e10 f10,v11 ~ Column a11 b11 c11 d11 e11 f11,v12 ~ Column a12 b12 c12 d12 e12 f12,v13 ~ Column a13 b13 c13 d13 e13 f13) => IsColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13) where
    type ToColumnsType (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13) = '[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13]
    toAnyColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3,AnyColumn v4,AnyColumn v5,AnyColumn v6,AnyColumn v7,AnyColumn v8,AnyColumn v9,AnyColumn v10,AnyColumn v11,AnyColumn v12,AnyColumn v13]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3,v4 ~ Column a4 b4 c4 d4 e4 f4,v5 ~ Column a5 b5 c5 d5 e5 f5,v6 ~ Column a6 b6 c6 d6 e6 f6,v7 ~ Column a7 b7 c7 d7 e7 f7,v8 ~ Column a8 b8 c8 d8 e8 f8,v9 ~ Column a9 b9 c9 d9 e9 f9,v10 ~ Column a10 b10 c10 d10 e10 f10,v11 ~ Column a11 b11 c11 d11 e11 f11,v12 ~ Column a12 b12 c12 d12 e12 f12,v13 ~ Column a13 b13 c13 d13 e13 f13,v14 ~ Column a14 b14 c14 d14 e14 f14) => IsColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14) where
    type ToColumnsType (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14) = '[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14]
    toAnyColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3,AnyColumn v4,AnyColumn v5,AnyColumn v6,AnyColumn v7,AnyColumn v8,AnyColumn v9,AnyColumn v10,AnyColumn v11,AnyColumn v12,AnyColumn v13,AnyColumn v14]
instance (v0 ~ Column a0 b0 c0 d0 e0 f0,v1 ~ Column a1 b1 c1 d1 e1 f1,v2 ~ Column a2 b2 c2 d2 e2 f2,v3 ~ Column a3 b3 c3 d3 e3 f3,v4 ~ Column a4 b4 c4 d4 e4 f4,v5 ~ Column a5 b5 c5 d5 e5 f5,v6 ~ Column a6 b6 c6 d6 e6 f6,v7 ~ Column a7 b7 c7 d7 e7 f7,v8 ~ Column a8 b8 c8 d8 e8 f8,v9 ~ Column a9 b9 c9 d9 e9 f9,v10 ~ Column a10 b10 c10 d10 e10 f10,v11 ~ Column a11 b11 c11 d11 e11 f11,v12 ~ Column a12 b12 c12 d12 e12 f12,v13 ~ Column a13 b13 c13 d13 e13 f13,v14 ~ Column a14 b14 c14 d14 e14 f14,v15 ~ Column a15 b15 c15 d15 e15 f15) => IsColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15) where
    type ToColumnsType (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15) = '[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15]
    toAnyColumns (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15) = [AnyColumn v0,AnyColumn v1,AnyColumn v2,AnyColumn v3,AnyColumn v4,AnyColumn v5,AnyColumn v6,AnyColumn v7,AnyColumn v8,AnyColumn v9,AnyColumn v10,AnyColumn v11,AnyColumn v12,AnyColumn v13,AnyColumn v14,AnyColumn v15]

columns
    :: IsColumns columns
    => columns
    -> Columns (ToColumnsType columns)
columns = Columns . toAnyColumns

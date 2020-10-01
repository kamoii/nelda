{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Database.Nelda.ColType where

import Database.Nelda.SqlColumnType(SqlColumnTypeRep, SqlColumnType(..))

import Data.Kind (Type)

-- * ColType

data ColType (ct :: p) (st :: Type) = ColType SqlColumnTypeRep

-- * _type/asSqlType Helpers

-- _type の使いかたは安全ではない(unsafe preifx 付けるか？)
-- ライブラリを実装する上での注意点だが, SqlColumnTypeRep は ct に対応するものでないといけない。
-- TODO: これ修正するか？
-- いやライブラリだけの安全性を無理して得る必要はないかな。
-- 実装するなら SqlColumnType 型クラスに type class を追加する必要が出てくる
_type :: SqlColumnType ct => SqlColumnTypeRep -> ColType ct (ToSqlType ct)
_type rep = ColType rep

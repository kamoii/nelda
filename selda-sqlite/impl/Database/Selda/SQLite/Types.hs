{-# LANGUAGE AllowAmbiguousTypes, OverloadedStrings #-}
module Database.Selda.SQLite.Types where

import Data.Typeable (Typeable)
import Control.Monad (void, when, unless)
import Control.Monad.Catch
import Data.ByteString.Lazy (toStrict)
import Data.Dynamic
import Data.Text as Text (pack, toLower, take)
import Data.Text (Text)
import Data.Time (FormatTime, formatTime, defaultTimeLocale)
import Data.UUID.Types (toByteString)
import Database.SQLite3
import System.Directory (makeAbsolute)

{-
sqlite の場合, LiteralType も ResultType も同じ SQLData
-}

-- | SQL backend にリテラルを渡す際の型
type LiteralType = SQLData

-- | NULL literal
-- | Needs for implementation of Lit a -> Literal
nullLiteral :: LiteralType
nullLiteral = SQLNull

-- | SQL backend のクエリの結果絵られるデータの型
type ResultType = SQLData

-- | 結果が NULL かどうかの判定。
-- instance SqlType (Maybe a) の実装で必要
isResultNull :: ResultType -> Bool
isResultNull SQLNull = True
isResultNull _       = False

-- | Debug用途
inspectResult :: ResultType -> Text
inspectResult _ = "TODO"

-- | Representation of an SQL type.
-- 通常 TText, TInt, TDateTime のような形式
-- TRowID は SQL backend の型ではなく, selda の都合上導入されていた論理型なので不要
data SqlTypeRep
    = TInteger
    | TFloat
    | TText
    | TBlob
    deriving (Show, Eq, Ord)

-- | Any datatype representable in SQL.
-- 実際にサポートされている型のみ。
-- NULL や 抽象化された ID は SqlType 型クラス('なし)で考慮する。
-- というかここでは instance 定義はできあにので,全ての backend で Maybe a や ID a の instance 実装が必要になる。
-- あと SqlType, SqlType' を分けないと orphan instance の問題が。
-- まあ関連ライブラリのみに閉じているからいいかもだけど...
--
-- TDOO: SqlType, SqlType' を分けているのが吉と出るか...
-- TODO: Typeable 制約は SqlType のために付けているだけで後から外せるかも
class Typeable a => SqlType' a where
    -- | Create a literal of this type.
    toLiteral :: a -> LiteralType
    -- | The SQL representation for this type.
    sqlTypeRep :: SqlTypeRep
    -- | Convert an SqlValue into this type.
    fromResult :: ResultType -> a
    -- | Pring for insperct purpose
    inspectPrint :: a -> Text
    -- | Default value when using 'def' at this type.
    -- TODO: DEPRECATE。DEFAULTカラムの insert 時に使っているがこれは本来ライブラリが決めるべき値ではない。
    defaultValue :: a

{-
微妙なところ。
例えば SQLite はカラム定義として Bool はないが, Int の 1/0 が true/false として扱われている。
selda は SQL構築する際に Col s Bool を使っており, SqlType Bool がないと Database.Selda モジュールのコンパイルが通らない。
厳密に言うと カラムが取れる型と SQL中の型は一致しない。
ただ厳密にやりすぎると ergnomics が下がるため取りあえず以下のものはどのバックエンドも最低限の要求とする。
-}
instance SqlType' Int where
    toLiteral i = SQLInteger $ fromIntegral i -- TODO: いいのか？
    sqlTypeRep = TInteger
    fromResult (SQLInteger i) = fromIntegral i  -- TODO: いいのか？
    inspectPrint = pack . show
    defaultValue = 0

instance SqlType' Text where
    toLiteral t = SQLText t
    sqlTypeRep = TText
    fromResult (SQLText t) = t
    inspectPrint = id
    defaultValue = ""

instance SqlType' Double where
    toLiteral d = SQLFloat d
    sqlTypeRep = TFloat
    fromResult (SQLFloat d) = d
    inspectPrint = pack . show
    defaultValue = 0.0

instance SqlType' Bool where
    toLiteral b = SQLInteger $ if b then 1 else 0
    sqlTypeRep = TInteger
    fromResult (SQLInteger i) = not (i==0)
    inspectPrint = pack . show
    defaultValue = False   -- TODO: やっぱ defaultValue って決まらんわ

-- | Any column type that can be used with the 'min_' and 'max_' functions.
-- | Int
class SqlType' a => SqlOrdable a

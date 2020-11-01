{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Database.Nelda.Query.Columns where

import Control.Monad.State.Strict (State, execState, modify, replicateM, runState, state)
import Data.Data (Proxy (Proxy))
import qualified Data.List as List
import Database.Nelda.Query.ResultRow (ResultRow, nestedCols)
import Database.Nelda.SQL.Col (Col (One))
import Database.Nelda.SQL.Row (Row (Many))
import Database.Nelda.SQL.Types (ColName, Exp (Col), UntypedCol (Untyped))

-- | Any column tuple.
-- ResultReader と同じ構造だな..
-- State [ColName] monad 使う？？
-- TODO: Its quite unasfes so be carefull. Maybe need some re-designs.
-- 例えば a ~ Row s ["foo" := Int, "bar" := String] の場合, [ColName] は foo, bar を参照する ColName である必要がある。
-- うーんというか :: Table name cols -> Row s (Rec lts) という関数を作ったほうがいいかも。
toTup :: Columns a => [ColName] -> a
toTup xs = case runState _toTup xs of
    (a, []) -> a
    (_, _) -> error "too many elements"

fromTup :: Columns a => a -> [UntypedCol]
fromTup a = List.reverse $ execState (_fromTup a) []

_takeOne :: State [x] x
_takeOne = state $ \s -> case s of
    (x : xs) -> (x, xs)
    _ -> error "too few elements"

-- TODO: tup という名前は微妙かもしれない...
-- NOTE: _fromTup は逆順に追加することに注意
class Columns a where
    _toTup :: State [ColName] a
    _fromTup :: a -> State [UntypedCol] ()

instance Columns (Col s n a) where
    _toTup = One . Col <$> _takeOne
    _fromTup (One x) = modify (Untyped x :)

instance ResultRow a => Columns (Row s n a) where
    _toTup =
        Many . map (Untyped . Col)
            <$> replicateM (nestedCols (Proxy :: Proxy a)) _takeOne
    _fromTup (Many xs) = modify $ \rs -> List.foldl' (\xs' x -> x : xs') rs xs

-- * Tuple instances(Up to 8-tuple)

-- 8 以上はちゃんと名前を付けて Row に入れるべきかな。
-- その意味で Column1 と Columns1 は分けるべきかも
-- んで :*: は廃止。というか

{-
def tuple_instance(n)
  vars = (0...n).map {|i| "c#{i}" }
  tuples = "(#{vars.join(',')})"
  constraint = vars.map {|v| "Columns #{v}"}.join(',')
  tuples_sec = "(#{vars.map{''}.join(',')})"
  to_tups = vars.map {|v| "_toTup @#{v}"}.join(' <*> ')
  from_tups = vars.map {|v| "_fromTup #{v}"}.join(' *> ')
  <<EOS
instance (#{constraint}) => Columns #{tuples} where
    _toTup = #{tuples_sec} <$> #{to_tups}
    _fromTup #{tuples} = #{from_tups}
EOS
end

2.upto(8) { |n| puts tuple_instance(n) }
-}

instance (Columns c0, Columns c1) => Columns (c0, c1) where
    _toTup = (,) <$> _toTup @c0 <*> _toTup @c1
    _fromTup (c0, c1) = _fromTup c0 *> _fromTup c1
instance (Columns c0, Columns c1, Columns c2) => Columns (c0, c1, c2) where
    _toTup = (,,) <$> _toTup @c0 <*> _toTup @c1 <*> _toTup @c2
    _fromTup (c0, c1, c2) = _fromTup c0 *> _fromTup c1 *> _fromTup c2
instance (Columns c0, Columns c1, Columns c2, Columns c3) => Columns (c0, c1, c2, c3) where
    _toTup = (,,,) <$> _toTup @c0 <*> _toTup @c1 <*> _toTup @c2 <*> _toTup @c3
    _fromTup (c0, c1, c2, c3) = _fromTup c0 *> _fromTup c1 *> _fromTup c2 *> _fromTup c3
instance (Columns c0, Columns c1, Columns c2, Columns c3, Columns c4) => Columns (c0, c1, c2, c3, c4) where
    _toTup = (,,,,) <$> _toTup @c0 <*> _toTup @c1 <*> _toTup @c2 <*> _toTup @c3 <*> _toTup @c4
    _fromTup (c0, c1, c2, c3, c4) = _fromTup c0 *> _fromTup c1 *> _fromTup c2 *> _fromTup c3 *> _fromTup c4
instance (Columns c0, Columns c1, Columns c2, Columns c3, Columns c4, Columns c5) => Columns (c0, c1, c2, c3, c4, c5) where
    _toTup = (,,,,,) <$> _toTup @c0 <*> _toTup @c1 <*> _toTup @c2 <*> _toTup @c3 <*> _toTup @c4 <*> _toTup @c5
    _fromTup (c0, c1, c2, c3, c4, c5) = _fromTup c0 *> _fromTup c1 *> _fromTup c2 *> _fromTup c3 *> _fromTup c4 *> _fromTup c5
instance (Columns c0, Columns c1, Columns c2, Columns c3, Columns c4, Columns c5, Columns c6) => Columns (c0, c1, c2, c3, c4, c5, c6) where
    _toTup = (,,,,,,) <$> _toTup @c0 <*> _toTup @c1 <*> _toTup @c2 <*> _toTup @c3 <*> _toTup @c4 <*> _toTup @c5 <*> _toTup @c6
    _fromTup (c0, c1, c2, c3, c4, c5, c6) = _fromTup c0 *> _fromTup c1 *> _fromTup c2 *> _fromTup c3 *> _fromTup c4 *> _fromTup c5 *> _fromTup c6
instance (Columns c0, Columns c1, Columns c2, Columns c3, Columns c4, Columns c5, Columns c6, Columns c7) => Columns (c0, c1, c2, c3, c4, c5, c6, c7) where
    _toTup = (,,,,,,,) <$> _toTup @c0 <*> _toTup @c1 <*> _toTup @c2 <*> _toTup @c3 <*> _toTup @c4 <*> _toTup @c5 <*> _toTup @c6 <*> _toTup @c7
    _fromTup (c0, c1, c2, c3, c4, c5, c6, c7) = _fromTup c0 *> _fromTup c1 *> _fromTup c2 *> _fromTup c3 *> _fromTup c4 *> _fromTup c5 *> _fromTup c6 *> _fromTup c7

-- * Data.Tup instance(For arbitrary length) *DEPRECATED*

--
-- ((a :*: b) :*: (c :*: d)) も受け入れてしまうな...
-- まあいいんだけど...

-- instance (Columns x, Columns xs) => Columns (x :*: xs) where
--     _toTup = (:*:) <$> _toTup @x <*> _toTup @xs
--     _fromTup (x :*: xs) = _fromTup x *> _fromTup xs

-- instance (ResultRow a, Columns b) => Columns (Row s a :*: b) where
--     _toTup xs =
--         case nestedCols (Proxy :: Proxy a) of
--             n -> Many (map (Untyped . Col) (take n xs)) :*: _toTup (drop n xs)
--     _fromTup (Many xs :*: xss) = xs ++ _fromTup xss

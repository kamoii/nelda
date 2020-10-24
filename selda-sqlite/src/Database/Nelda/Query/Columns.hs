module Database.Nelda.Query.Columns where


-- | Any column tuple.
class Columns a where
    toTup :: [ColName] -> a
    fromTup :: a -> [UntypedCol]

instance (SqlType a, Columns b) => Columns (Col s a :*: b) where
    toTup (x:xs) = One (Col x) :*: toTup xs
    toTup []     = error "too few elements to toTup"
    fromTup (One x :*: xs) = Untyped x : fromTup xs

instance (ResultRow a, Columns b) => Columns (Row s a :*: b) where
    toTup xs =
        case nestedCols (Proxy :: Proxy a) of
            n -> Many (map (Untyped . Col) (take n xs)) :*: toTup (drop n xs)
    fromTup (Many xs :*: xss) = xs ++ fromTup xss

instance Columns (Col s a) where
    toTup [x] = One (Col x)
    toTup []  = error "too few elements to toTup"
    toTup _   = error "too many elements to toTup"
    fromTup (One x) = [Untyped x]

instance Columns (Row s a) where
    toTup xs = Many (map (Untyped . Col) xs)
    fromTup (Many xs) = xs

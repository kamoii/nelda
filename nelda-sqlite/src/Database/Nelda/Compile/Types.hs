module Database.Nelda.Compile.Types where


-- IF NOT EXISTS を付けるか否か
-- Boolean blindness を避けるため
-- 名前難しい

data ExistenceCheck
    = IgnoreExistence
    | ConcernExistence
    deriving (Eq, Ord, Show)

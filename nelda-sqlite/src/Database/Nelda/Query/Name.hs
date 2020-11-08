module Database.Nelda.Query.Name where

type Scope = Int
type Ident = Int

-- | A name, consisting of a scope and an identifier.
data Name = Name Scope Ident

instance Show Name where
    show (Name 0 n) = concat [show n]
    show (Name s n) = concat [show s, "s_", show n]

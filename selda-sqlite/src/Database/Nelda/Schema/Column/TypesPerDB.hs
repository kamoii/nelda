module Database.Nelda.Schema.Column.TypesPerDB where


-- NOT NULL/DEFAULT 以外のもの

data ColumnConstraint
    = CCPrimaryKey
    | CCUnique
    deriving (Eq, Show)

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Selda.Backend.Class where

import Data.Proxy (Proxy)
import Database.Selda.Types (Inner)

class Backend b where
    type BackendLitType b
    data BackendTypeRep b

instance Backend b => Backend (Inner b) where
    type BackendLitType (Inner b) = BackendLitType b
    data BackendTypeRep (Inner b) = BackendTypeRep b

-- Backend で実装する前提
class Backend b => SqlType b st where
    backendLit :: st -> BackendLitType b
    backendTypeRep :: Proxy st -> BackendTypeRep b

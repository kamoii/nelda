module Database.Selda.PostgreSQL.Oid where

import Database.PostgreSQL.LibPQ (Oid (..))

-- | OIDs for all types used by Selda.
blobType, boolType, intType, int32Type, int16Type, textType, doubleType,
  dateType, timeType, timestampType, nameType, varcharType, uuidType,
  jsonbType :: Oid
boolType      = Oid 16
intType       = Oid 20
int32Type     = Oid 23
int16Type     = Oid 21
textType      = Oid 25
nameType      = Oid 19
doubleType    = Oid 701
dateType      = Oid 1082
timeType      = Oid 1266
timestampType = Oid 1184
blobType      = Oid 17
varcharType   = Oid 1043
uuidType      = Oid 2950
jsonbType     = Oid 3802

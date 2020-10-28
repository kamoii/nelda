{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, CPP #-}
-- | Utility functions that are useful for all tests.
module Utils where
import Control.Monad.Catch
import Data.Text (unpack)
#ifdef POSTGRES
import Database.Nelda.PostgreSQL
import Database.Nelda.PostgreSQL.Debug
#endif
import Test.HUnit
import Database.Nelda.Backend.Monad (NeldaM)
import Database.Nelda.Query.Result (Result(Res))
import Database.Nelda.Query.Monad (Query)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Database.Nelda.Action (query)
import Database.Nelda.Compile.Query (compileQuery)
import Data.Coerce (coerce)
import Database.Nelda.Types (Sql(Sql))
import GHC.Records.Compat (HasField(..))
import qualified JRec.Internal as JRec

-- | HasField isntance for Rec.
-- Till real RecordDotSyntax lands to GHC.

instance (JRec.Has l lts t, JRec.Set l lts t ~ lts) => HasField l (JRec.Rec lts) t where
    hasField r =
        ( \t -> JRec.set (JRec.FldProxy :: JRec.FldProxy l) t r
        , JRec.get (JRec.FldProxy :: JRec.FldProxy l) r
        )

-- | Assert that the given computation should fail.
assertFail :: NeldaM a -> NeldaM ()
assertFail m = do
    res <- try m
    case res of
        Left (SomeException _) -> return ()
        _                      -> liftIO $ assertFailure "computation did not fail"

-- | @NeldaT@ wrapper for 'assertEqual'.
assEq :: (Show a, Eq a) => String -> a -> a -> NeldaM ()
assEq s expect actual = liftIO $ assertEqual s expect actual

-- | @NeldaT@ wrapper for 'assertEqual'.
assQueryEq :: (Result a, Show (Res a), Eq (Res a)) => String -> [Res a] -> Query s a -> NeldaM ()
assQueryEq s expect q = do
    eactual <- try $! query q >>= mapM (\x -> pure $! x)
    let msg = "Generated query:\n" ++ unpack (coerce $ fst $ compileQuery q) ++ "\n"
    case eactual of
        Right actual ->
            liftIO $ assertEqual (s ++ "\n" ++ msg) expect actual
        Left (SomeException e) ->
            ass (msg ++ "\nException thrown:\n" ++ show e ++ "\n") False

-- | @NeldaT@ wrapper for 'assertBool'.
ass :: String -> Bool -> NeldaM ()
ass s pred = liftIO $ assertBool s pred

{-# LANGUAGE DataKinds, TypeFamilies, RankNTypes, UndecidableInstances, TemplateHaskell, RecordWildCards #-}

{-# OPTIONS -ddump-splices #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Reader
import Monad.Capabilities

-------- Effect declarations ----------

data Logging m = Logging
  { _logError :: String -> m (),
    _logWarning :: String -> m ()
  }

makeCap ''Logging

instance
  (HasCap Logging caps, HasCapabilities r caps m) =>
    MonadLogging (ReaderT r m)
  where
    logError msg = ReaderT $ \caps -> _logError (toCap caps) msg
    logWarning msg = ReaderT $ \caps -> _logWarning (toCap caps) msg

data DB m = DB
  { _dbGet :: String -> m String,
    _dbPut :: String -> String -> m (),
    _dbWithLock :: forall a. (String -> m () -> m a) -> m a
  }

makeCap ''DB

instance
  (HasCap DB caps, HasCapabilities r caps m, Monad m) =>
    MonadDB (ReaderT r m)
  where
    dbGet key = ReaderT $ \caps -> _dbGet (toCap caps) key
    dbPut key value = ReaderT $ \caps -> _dbPut (toCap caps) key value
    dbWithLock cont = ReaderT $ \caps ->
      _dbWithLock (toCap caps) $ \s act ->
        runReaderT (cont s (lift act)) caps

-------- Effect implementations ----------

loggingDummy :: Monad m => CapImpl Logging '[] m
loggingDummy = CapImpl $ \_ -> Logging
  { _logError = \_ -> return (),
    _logWarning = \_ -> return ()
  }

loggingIO :: MonadIO m => CapImpl Logging '[Logging] m
loggingIO = CapImpl $ \caps -> Logging
  { _logError = liftIO . putStrLn,
    _logWarning =
      let Logging{..} = toCap caps
      in \msg -> _logError msg -- recursive use of capabilities!
  }

dbDummy :: Monad m => CapImpl DB '[Logging] m
dbDummy = CapImpl $ \caps -> DB
  { _dbGet = \key ->
      let Logging{..} = toCap caps
      in do _logWarning ("get " ++ key); return "v",
    _dbPut = \key value ->
      let Logging{..} = toCap caps
      in do _logWarning ("put " ++ key ++ " " ++ value); return (),
    _dbWithLock =
      let Logging{..} = toCap caps
      in \m -> m "lock" (_logWarning "lock")
  }

-------- Test implementations ----------

testLoggingOverride :: TestTree
testLoggingOverride = testCase "logging override" $ do
  let
    caps = buildCaps $
      AddCap loggingIO $ -- try commenting out this line,
                         -- you get a nice error message
      -- AddCap loggingDummy $ -- try uncommenting this line,
                               -- you get a nice error message
      AddCap dbDummy $
      BaseCaps emptyCaps
  flip runReaderT caps $ do
    v <- dbGet "k" -- will have log output
    withReaderT (overrideCap loggingDummy) $ do
      dbPut "k2" v -- will not have log output
  -- I KNOW THIS IS NOT A PROPER UNIT TEST :)
  -- Check the output in the console manually for now.

testAddingDb :: TestTree
testAddingDb = testCase "adding db" $ do
  let
    caps = buildCaps $
      AddCap loggingIO $
      BaseCaps emptyCaps
  flip runReaderT caps $ do
    -- can't have DB access here
    withReaderT (insertCap dbDummy) $ do
      -- have DB access here
      dbPut "k" "v"
  -- I KNOW THIS IS NOT A PROPER UNIT TEST :)
  -- Check the output in the console manually for now.


-------- Test tree and Main ----------

main :: IO ()
main = do
  defaultMain suite

suite :: TestTree
suite = testGroup "Capabilities"
  [ testLoggingOverride,
    testAddingDb
  ]

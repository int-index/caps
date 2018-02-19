{-# LANGUAGE DataKinds, TypeFamilies, RankNTypes, UndecidableInstances, TemplateHaskell #-}

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

data DB m = DB
  { _dbGet :: String -> m String,
    _dbPut :: String -> String -> m (),
    _dbWithLock :: forall a. (String -> m a) -> m a
  }

makeCap ''DB

-------- Effect implementations ----------

loggingDummy :: Monad m => CapImpl Logging '[] m
loggingDummy = CapImpl $ Logging
  { _logError = \_ -> return (),
    _logWarning = \_ -> return ()
  }

loggingIO :: MonadIO m => CapImpl Logging '[Logging] m
loggingIO = CapImpl $ Logging
  { _logError = liftIO . putStrLn,
    _logWarning = logError -- recursive use of capabilities!
  }

dbDummy :: Monad m => CapImpl DB '[Logging] m
dbDummy = CapImpl $ DB
  { _dbGet = \key -> do logWarning ("get " ++ key); return "v",
    _dbPut = \key value -> do logWarning ("put " ++ key ++ " " ++ value); return (),
    _dbWithLock = \m -> m "lock"
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

{-# LANGUAGE DataKinds, TypeFamilies, RankNTypes, UndecidableInstances,
             MultiParamTypeClasses, FlexibleInstances, TypeApplications,
             AllowAmbiguousTypes, ScopedTypeVariables, TemplateHaskell #-}

{-# OPTIONS -ddump-splices #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Reader
import Monad.Capabilities

-------- Effect declarations ----------

data Logging msg m = Logging
  { _logError :: msg -> m (),
    _logWarning :: msg -> m ()
  }

makeCap ''Logging

data DB k v m = DB
  { _dbGet :: k -> m v,
    _dbPut :: k -> v -> m (),
    _dbWithLock :: forall a. (String -> m a) -> m a
  }

makeCap ''DB

-------- Effect implementations ----------

loggingDummy :: forall msg m. Monad m => CapImpl (Logging msg) '[] m
loggingDummy = CapImpl $ Logging
  { _logError = \_ -> return (),
    _logWarning = \_ -> return ()
  }

loggingIO :: MonadIO m => CapImpl (Logging String) '[Logging String] m
loggingIO = CapImpl $ Logging
  { _logError = liftIO . putStrLn,
    _logWarning = logError -- recursive use of capabilities!
  }

dbDummy :: Monad m => CapImpl (DB String Integer) '[Logging String] m
dbDummy = CapImpl $ DB
  { _dbGet = \key -> do logWarning ("get " ++ key); return 0,
    _dbPut = \key value -> do logWarning ("put " ++ key ++ " " ++ show value); return (),
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
    v :: Integer <- dbGet "k" -- will have log output
    withReaderT (overrideCap @(Logging String) loggingDummy) $ do
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
      dbPut "k" (42 :: Integer)
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

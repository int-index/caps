module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Reader
import Monad.Capabilities

main :: IO ()
main = do
  defaultMain suite

suite :: TestTree
suite = testGroup "Capabilities"
  [ testLoggingOverride,
    testAddingDb
  ]

testLoggingOverride :: TestTree
testLoggingOverride = testCase "logging override" $ do
  let
    caps = initCaps $
      AddCap loggingIO $ -- try commenting out this line,
                         -- you get a nice error message
      AddCap dbDummy $
      NoCaps
  flip runReaderT caps $ do
    v <- dbGet "k" -- will have log output
    withReaderT (overrideCap loggingDummy) $ do
      dbPut "k2" v -- will not have log output
  -- I KNOW THIS IS NOT A PROPER UNIT TEST :)
  -- Check the output in the console manually for now.

testAddingDb :: TestTree
testAddingDb = testCase "adding db" $ do
  let
    caps = initCaps $
      AddCap loggingIO $
      NoCaps
  flip runReaderT caps $ do
    -- can't have DB access here
    withReaderT (insertCap dbDummy) $ do
      -- have DB access here
      dbPut "k" "v"
  -- I KNOW THIS IS NOT A PROPER UNIT TEST :)
  -- Check the output in the console manually for now.

-------- Effect declarations ----------

data Logging m = Logging
  { _logError :: String -> m (),
    _logWarning :: String -> m ()
  }

instance Coercible1 Logging where
  coerce1 = Coercion

logError :: (Monad m, HasCap Logging caps) => String -> CapsT i caps m ()
logError message = withCap $ \cap -> _logError cap message

logWarning :: (Monad m, HasCap Logging caps) => String -> CapsT i caps m ()
logWarning message = withCap $ \cap -> _logWarning cap message

data DB m = DB
  { _dbGet :: String -> m String,
    _dbPut :: String -> String -> m ()
  }

instance Coercible1 DB where
  coerce1 = Coercion

dbGet :: (Monad m, HasCap DB caps) => String -> CapsT i caps m String
dbGet key = withCap $ \cap -> _dbGet cap key

dbPut :: (Monad m, HasCap DB caps) => String -> String -> CapsT i caps m ()
dbPut key val = withCap $ \cap -> _dbPut cap key val

-------- Effect implementations ----------

loggingDummy :: Monad m => Logging m
loggingDummy = Logging
  { _logError = \_ -> return (),
    _logWarning = \_ -> return ()
  }

loggingIO :: (MonadIO m, HasCap Logging caps) => Logging (CapsT i caps m)
loggingIO = Logging
  { _logError = liftIO . putStrLn,
    _logWarning = logError -- recursive use of capabilities!
  }

dbDummy :: (Monad m, HasCap Logging caps) => DB (CapsT i caps m)
dbDummy = DB
  { _dbGet = \key -> do logWarning ("get " ++ key); return "v",
    _dbPut = \key value -> do logWarning ("put " ++ key ++ " " ++ value); return ()
  }

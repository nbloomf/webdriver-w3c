{- |
Module      : Test.Tasty.WebDriver
Description : WebDriver integration with the Tasty test framework.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Test.Tasty.WebDriver (
  -- * Test Case Constructors
    testCase
  , testCaseWithSetup

  -- * Options
  , WebDriverRemoteHost(..)
  , WebDriverLogHandle(..)
  , WebDriverAssertionLogHandle(..)
  , FileHandle(..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Typeable (Typeable, Proxy(Proxy))
import Data.List (unlines)
import qualified Test.Tasty.Providers as T
import qualified Test.Tasty.Options as TO
import System.IO

import Web.Api.Http
import Web.Api.WebDriver


data WebDriverTest m = WebDriverTest
  { _test_name :: Maybe String
  , _test_session :: (WebDriver m ())
  } deriving Typeable

instance (Effectful m, Typeable m) => T.IsTest (WebDriverTest m) where

  testOptions = return
    [ TO.Option (Proxy :: Proxy WebDriverRemoteHost)
    , TO.Option (Proxy :: Proxy WebDriverLogHandle)
    , TO.Option (Proxy :: Proxy WebDriverAssertionLogHandle)
    ]

  run opts WebDriverTest{..} _ = do
    let
      WebDriverRemoteHost host = TO.lookupOption opts
      WebDriverLogHandle log = TO.lookupOption opts
      WebDriverAssertionLogHandle alog = TO.lookupOption opts

    logHandle <- writeModeHandle log
    alogHandle <- writeModeHandle alog

    let
      title = case _test_name of
        Nothing -> return ()
        Just str -> comment str

      config =
        setEnv
          ( setLogHandle logHandle
          . setAssertionLogHandle alogHandle
          . setClientEnvironment
              ( setRemoteHostname host
              $ defaultWebDriverEnv
              )
          ) 
        defaultWebDriverConfig

      caps = emptyCapabilities

    results <- toIO $ debugSession config $ runIsolated caps (title >> _test_session)

    return $ webDriverAssertionsToResult $ summarize results


webDriverAssertionsToResult :: AssertionSummary -> T.Result
webDriverAssertionsToResult x =
  if numFailures x > 0
    then T.testFailed $ unlines $ map showAssertion $ failures x
    else T.testPassed $ show (numSuccesses x) ++ " assertion(s)"



-- | Simple WebDriver test case.
testCase
  :: (Effectful m, Typeable m)
  => T.TestName
  -> WebDriver m ()
  -> T.TestTree
testCase name test =
  testCaseWithSetup name (return ()) (return ()) test


-- | WebDriver test case with additional setup and teardown phases -- setup runs before the test (for e.g. logging in) and teardown runs after the test (for e.g. deleting temp files).
testCaseWithSetup
  :: (Effectful m, Typeable m)
  => T.TestName
  -> WebDriver m () -- ^ Setup
  -> WebDriver m () -- ^ Teardown
  -> WebDriver m () -- ^ The test
  -> T.TestTree
testCaseWithSetup name setup teardown test =
  T.singleTest name $ WebDriverTest
    { _test_name = Just name
    , _test_session = setup >> test >> teardown
    }



-- | Hostname of the remote end.
newtype WebDriverRemoteHost
  = WebDriverRemoteHost { theWebDriverRemoteHost :: String }
  deriving Typeable

instance TO.IsOption WebDriverRemoteHost where
  defaultValue = WebDriverRemoteHost "localhost"
  parseValue str = Just $ WebDriverRemoteHost str
  optionName = return "remote end hostname"
  optionHelp = return "default: localhost"



-- | Log location.
newtype WebDriverLogHandle
  = WebDriverLogHandle { theWebDriverLogHandle :: FileHandle }
  deriving Typeable

instance TO.IsOption WebDriverLogHandle where
  defaultValue = WebDriverLogHandle StdErr
  parseValue path = Just $ WebDriverLogHandle $ Path path
  optionName = return "log file name"
  optionHelp = return "default: stderr"



-- | Assertion log location.
newtype WebDriverAssertionLogHandle
  = WebDriverAssertionLogHandle { theWebDriverAssertionLogHandle :: FileHandle }
  deriving Typeable

instance TO.IsOption WebDriverAssertionLogHandle where
  defaultValue = WebDriverAssertionLogHandle StdOut
  parseValue path = Just $ WebDriverAssertionLogHandle $ Path path
  optionName = return "assertion log file name"
  optionHelp = return "default: stdout"



-- | Representation of file handles, both paths and the stdin/out/err handles.
data FileHandle
  = StdIn
  | StdErr
  | StdOut
  | Path FilePath

writeModeHandle :: FileHandle -> IO Handle
writeModeHandle x = case x of
  StdIn -> error "writeModeHandle: Cannot open stdin in write mode."
  StdOut -> return stdout
  StdErr -> return stderr
  Path path -> openFile path WriteMode

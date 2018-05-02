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

  , ignoreChromedriver

  -- * Options
  , Driver(..)
  , DriverName(..)
  , RemoteHost(..)
  , RemotePort(..)
  , RemotePath(..)
  , SecretsPath(..)
  , ApiResponseFormat(..)
  , WebDriverApiVersion(..)
  , LogHandle(..)
  , AssertionLogHandle(..)
  , ConsoleInHandle(..)
  , ConsoleOutHandle(..)
  , FileHandle(..)
  ) where

import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Data.Typeable
  ( Typeable, Proxy(Proxy) )
import Data.List
  ( unlines )
import System.IO
  ( Handle, stdout, stderr, stdin, openFile, IOMode(..) )
import qualified Test.Tasty as T
import qualified Test.Tasty.Providers as TT
import qualified Test.Tasty.Options as TO
import qualified Test.Tasty.ExpectedFailure as TE
import qualified System.Environment as SE
  ( getEnv )

import Web.Api.Http
import Web.Api.WebDriver


data WebDriverTest m = WebDriverTest
  { _test_name :: Maybe String
  , _test_session :: (WebDriver m ())
  } deriving Typeable

instance (Effectful m, Typeable m) => TT.IsTest (WebDriverTest m) where

  testOptions = return
    [ TO.Option (Proxy :: Proxy Driver)
    , TO.Option (Proxy :: Proxy Headless)
    , TO.Option (Proxy :: Proxy RemoteHost)
    , TO.Option (Proxy :: Proxy RemotePort)
    , TO.Option (Proxy :: Proxy RemotePath)
    , TO.Option (Proxy :: Proxy ApiResponseFormat)
    , TO.Option (Proxy :: Proxy WebDriverApiVersion)
    , TO.Option (Proxy :: Proxy LogHandle)
    , TO.Option (Proxy :: Proxy AssertionLogHandle)
    , TO.Option (Proxy :: Proxy ConsoleInHandle)
    , TO.Option (Proxy :: Proxy ConsoleOutHandle)
    , TO.Option (Proxy :: Proxy SecretsPath)
    ]

  run opts WebDriverTest{..} _ = do
    let
      Driver driver = TO.lookupOption opts
      Headless headless = TO.lookupOption opts
      RemoteHost host = TO.lookupOption opts
      RemotePort port = TO.lookupOption opts
      RemotePath path = TO.lookupOption opts
      ApiResponseFormat format = TO.lookupOption opts
      WebDriverApiVersion version = TO.lookupOption opts
      LogHandle log = TO.lookupOption opts
      AssertionLogHandle alog = TO.lookupOption opts
      ConsoleInHandle cin = TO.lookupOption opts
      ConsoleOutHandle cout = TO.lookupOption opts
      SecretsPath secrets = TO.lookupOption opts

    logHandle <- writeModeHandle log
    alogHandle <- writeModeHandle alog
    cinHandle <- readModeHandle cin
    coutHandle <- writeModeHandle cout

    secretsPath <- case secrets of
      "" -> fmap (++ "/.webdriver/secrets") $ SE.getEnv "HOME"
      spath -> return spath

    let
      title = case _test_name of
        Nothing -> return ()
        Just str -> comment str

      config =
        setEnv
          ( setLogHandle logHandle
          . setAssertionLogHandle alogHandle
          . setConsoleInHandle cinHandle
          . setConsoleOutHandle coutHandle
          . setClientEnvironment
              ( setRemoteHostname host
              . setRemotePort port
              . setRemotePath path
              . setResponseFormat format
              . setApiVersion version
              . setSecretsPath secretsPath
              $ defaultWebDriverEnv
              )
          ) defaultWebDriverConfig

      caps = case driver of
        Geckodriver -> emptyCapabilities
          { _browser_name = Just Firefox
          , _firefox_options = Just $ defaultFirefoxOptions
              { _firefox_args = if headless then Just ["-headless"] else Nothing
              }
          }

        Chromedriver -> emptyCapabilities
          { _browser_name = Just Chrome
          , _chrome_options = Just $ defaultChromeOptions
              { _chrome_args = if headless then Just ["--headless"] else Nothing
              }
          }

    (result, assertions) <- toIO $
      debugSession config $ title >> runIsolated caps _test_session

    return $ case result of
      Right _ -> webDriverAssertionsToResult $ summarize assertions
      Left err -> TT.testFailed $
        "Unhandled error: " ++ printErr printWebDriverError err


webDriverAssertionsToResult :: AssertionSummary -> TT.Result
webDriverAssertionsToResult x =
  if numFailures x > 0
    then TT.testFailed $ unlines $ map showAssertion $ failures x
    else TT.testPassed $ show (numSuccesses x) ++ " assertion(s)"



-- | Simple WebDriver test case.
testCase
  :: (Effectful m, Typeable m)
  => TT.TestName
  -> WebDriver m ()
  -> TT.TestTree
testCase name test =
  testCaseWithSetup name (return ()) (return ()) test


-- | WebDriver test case with additional setup and teardown phases -- setup runs before the test (for e.g. logging in) and teardown runs after the test (for e.g. deleting temp files).
testCaseWithSetup
  :: (Effectful m, Typeable m)
  => TT.TestName
  -> WebDriver m () -- ^ Setup
  -> WebDriver m () -- ^ Teardown
  -> WebDriver m () -- ^ The test
  -> TT.TestTree
testCaseWithSetup name setup teardown test =
  TT.singleTest name $ WebDriverTest
    { _test_name = Just name
    , _test_session = setup >> test >> teardown
    }



-- | Remote end name.
newtype Driver
  = Driver { theDriver :: DriverName }
  deriving Typeable

instance TO.IsOption Driver where
  defaultValue = Driver Geckodriver
  parseValue str = case str of
    "geckodriver" -> Just $ Driver Geckodriver
    "chromedriver" -> Just $ Driver Chromedriver
    _ -> Nothing
  optionName = return "driver name"
  optionHelp = return "default: geckodriver"



-- | Run in headless mode.
newtype Headless
  = Headless { theHeadless :: Bool }
  deriving Typeable

instance TO.IsOption Headless where
  defaultValue = Headless False
  parseValue = fmap Headless . TO.safeReadBool
  optionName = return "headless"
  optionHelp = return "run in headless mode (default: false)"



-- | Hostname of the remote end.
newtype RemoteHost
  = RemoteHost { theRemoteHost :: String }
  deriving Typeable

instance TO.IsOption RemoteHost where
  defaultValue = RemoteHost "localhost"
  parseValue str = Just $ RemoteHost str
  optionName = return "remote end hostname"
  optionHelp = return "default: localhost"



-- | Port of the remote end.
newtype RemotePort
  = RemotePort { theRemotePort :: Int }
  deriving Typeable

instance TO.IsOption RemotePort where
  defaultValue = RemotePort 4444
  parseValue = fmap RemotePort . TO.safeRead
  optionName = return "remote end port"
  optionHelp = return "default: 4444"



-- | Additional path of the remote end URL.
newtype RemotePath
  = RemotePath { theRemotePath :: String }
  deriving Typeable

instance TO.IsOption RemotePath where
  defaultValue = RemotePath ""
  parseValue = Just . RemotePath
  optionName = return "remote end path"
  optionHelp = return "default: (empty)"



-- | Path where secrets are stored.
newtype SecretsPath
  = SecretsPath { theSecretsPath :: FilePath }
  deriving Typeable

instance TO.IsOption SecretsPath where
  defaultValue = SecretsPath ""
  parseValue path = Just $ SecretsPath path
  optionName = return "secrets path"
  optionHelp = return "default: ~/.webdriver/secrets"



-- | Expected API response format.
newtype ApiResponseFormat
  = ApiResponseFormat { theApiResponseFormat :: ResponseFormat }
  deriving Typeable

instance TO.IsOption ApiResponseFormat where
  defaultValue = ApiResponseFormat SpecFormat
  parseValue str = case str of
    "spec" -> Just $ ApiResponseFormat SpecFormat
    "chromedriver" -> Just $ ApiResponseFormat ChromeFormat
    _ -> Nothing
  optionName = return "response format"
  optionHelp = return "defaul: spec"



-- | WebDriver API version.
newtype WebDriverApiVersion
  = WebDriverApiVersion { theWebDriverApiVersion :: ApiVersion }
  deriving Typeable

instance TO.IsOption WebDriverApiVersion where
  defaultValue = WebDriverApiVersion CR_2018_03_04
  parseValue str = case str of
    "cr-2018-03-04" -> Just $ WebDriverApiVersion CR_2018_03_04
    _ -> Nothing
  optionName = return "webdriver API version"
  optionHelp = return "cr-2018-03-04"



-- | Log location.
newtype LogHandle
  = LogHandle { theLogHandle :: FileHandle }
  deriving Typeable

instance TO.IsOption LogHandle where
  defaultValue = LogHandle StdErr
  parseValue path = Just $ LogHandle $ Path path
  optionName = return "log file name"
  optionHelp = return "default: stderr"



-- | Assertion log location.
newtype AssertionLogHandle
  = AssertionLogHandle { theAssertionLogHandle :: FileHandle }
  deriving Typeable

instance TO.IsOption AssertionLogHandle where
  defaultValue = AssertionLogHandle StdOut
  parseValue path = Just $ AssertionLogHandle $ Path path
  optionName = return "assertion log file name"
  optionHelp = return "default: stdout"



-- | Console in location.
newtype ConsoleInHandle
  = ConsoleInHandle { theConsoleInHandle :: FileHandle }
  deriving Typeable

instance TO.IsOption ConsoleInHandle where
  defaultValue = ConsoleInHandle StdIn
  parseValue path = Just $ ConsoleInHandle $ Path path
  optionName = return "console input file name"
  optionHelp = return "default: stdin"



-- | Console out location.
newtype ConsoleOutHandle
  = ConsoleOutHandle { theConsoleOutHandle :: FileHandle }
  deriving Typeable

instance TO.IsOption ConsoleOutHandle where
  defaultValue = ConsoleOutHandle StdOut
  parseValue path = Just $ ConsoleOutHandle $ Path path
  optionName = return "console out file name"
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

readModeHandle :: FileHandle -> IO Handle
readModeHandle x = case x of
  StdIn -> return stdin
  StdOut -> error "readModeHandle: Cannot open stdout in read mode."
  StdErr -> error "readModeHandle: Cannot open stderr in read mode."
  Path path -> openFile path ReadMode



-- | Remote end name.
data DriverName
  = Geckodriver
  | Chromedriver
  deriving Typeable



-- | Ignore a test tree if the @Driver@ option is "chromedriver". Useful because chromedriver doesn't implement all endpoints.
ignoreChromedriver :: TT.TestTree -> TT.TestTree
ignoreChromedriver tree = T.askOption failure
  where
    failure :: Driver -> TT.TestTree
    failure (Driver d) = case d of
      Geckodriver -> tree
      Chromedriver -> TE.ignoreTest tree

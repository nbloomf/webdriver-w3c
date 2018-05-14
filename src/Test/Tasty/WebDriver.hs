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

  , ifDriverIs
  , unlessDriverIs
  , ifTierIs
  , unlessTierIs

  -- * Options
  , Driver(..)
  , DriverName(..)
  , RemoteHost(..)
  , RemotePort(..)
  , RemotePath(..)
  , SecretsPath(..)
  , Deployment(..)
  , DeploymentTier(..)
  , BrowserPath(..)
  , ApiResponseFormat(..)
  , WebDriverApiVersion(..)
  , LogHandle(..)
  , LogNoiseLevel(..)
  , AssertionLogHandle(..)
  , ConsoleInHandle(..)
  , ConsoleOutHandle(..)
  , FileHandle(..)
  , Headless(..)
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
    , TO.Option (Proxy :: Proxy LogNoiseLevel)
    , TO.Option (Proxy :: Proxy AssertionLogHandle)
    , TO.Option (Proxy :: Proxy ConsoleInHandle)
    , TO.Option (Proxy :: Proxy ConsoleOutHandle)
    , TO.Option (Proxy :: Proxy Deployment)
    , TO.Option (Proxy :: Proxy SecretsPath)
    , TO.Option (Proxy :: Proxy BrowserPath)
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
      logNoiseLevel = TO.lookupOption opts
      AssertionLogHandle alog = TO.lookupOption opts
      ConsoleInHandle cin = TO.lookupOption opts
      ConsoleOutHandle cout = TO.lookupOption opts
      SecretsPath secrets = TO.lookupOption opts
      BrowserPath browserPath = TO.lookupOption opts

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

      logNoise = case logNoiseLevel of
        NoisyLog -> noisyLog
        SilentLog -> silentLog

      config =
        setEnv
          ( setLogHandle logHandle
          . setLogVerbosity logNoise
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
              { _firefox_binary = browserPath
              , _firefox_args = if headless then Just ["-headless"] else Nothing
              }
          }

        Chromedriver -> emptyCapabilities
          { _browser_name = Just Chrome
          , _chrome_options = Just $ defaultChromeOptions
              { _chrome_binary = browserPath
              , _chrome_args = if headless then Just ["--headless"] else Nothing
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
  optionName = return "driver"
  optionHelp = return "remote end name: (geckodriver), chromedriver"



-- | Run in headless mode.
newtype Headless
  = Headless { theHeadless :: Bool }
  deriving Typeable

instance TO.IsOption Headless where
  defaultValue = Headless False
  parseValue = fmap Headless . TO.safeReadBool
  optionName = return "headless"
  optionHelp = return "run in headless mode: (false), true"



-- | Hostname of the remote end.
newtype RemoteHost
  = RemoteHost { theRemoteHost :: String }
  deriving Typeable

instance TO.IsOption RemoteHost where
  defaultValue = RemoteHost "localhost"
  parseValue str = Just $ RemoteHost str
  optionName = return "remotehost"
  optionHelp = return "remote end hostname: (localhost), STR"



-- | Port of the remote end.
newtype RemotePort
  = RemotePort { theRemotePort :: Int }
  deriving Typeable

instance TO.IsOption RemotePort where
  defaultValue = RemotePort 4444
  parseValue = fmap RemotePort . TO.safeRead
  optionName = return "remoteport"
  optionHelp = return "remote end port: (4444), INT"



-- | Additional path of the remote end URL.
newtype RemotePath
  = RemotePath { theRemotePath :: String }
  deriving Typeable

instance TO.IsOption RemotePath where
  defaultValue = RemotePath ""
  parseValue = Just . RemotePath
  optionName = return "remotepath"
  optionHelp = return "remote end path: (), STR"



-- | Path where secrets are stored.
newtype SecretsPath
  = SecretsPath { theSecretsPath :: FilePath }
  deriving Typeable

instance TO.IsOption SecretsPath where
  defaultValue = SecretsPath ""
  parseValue path = Just $ SecretsPath path
  optionName = return "secrets"
  optionHelp = return "secrets path: (~/.webdriver/secrets), PATH"



-- | Path to browser binary.
newtype BrowserPath
  = BrowserPath { theBrowserPath :: Maybe FilePath }
  deriving Typeable

instance TO.IsOption BrowserPath where
  defaultValue = BrowserPath Nothing
  parseValue path = Just $ BrowserPath $ Just path
  optionName = return "browserpath"
  optionHelp = return "path to browser binary: (), PATH"



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
  optionName = return "response-format"
  optionHelp = return "JSON response format: (spec), chromedriver"



-- | WebDriver API version.
newtype WebDriverApiVersion
  = WebDriverApiVersion { theWebDriverApiVersion :: ApiVersion }
  deriving Typeable

instance TO.IsOption WebDriverApiVersion where
  defaultValue = WebDriverApiVersion CR_2018_03_04
  parseValue str = case str of
    "cr-2018-03-04" -> Just $ WebDriverApiVersion CR_2018_03_04
    _ -> Nothing
  optionName = return "api-version"
  optionHelp = return "WebDriver API version: (cr-2018-03-04)"



-- | Log location.
newtype LogHandle
  = LogHandle { theLogHandle :: FileHandle }
  deriving Typeable

instance TO.IsOption LogHandle where
  defaultValue = LogHandle StdErr
  parseValue path = case path of
    "stdout" -> Just $ LogHandle StdOut
    "stderr" -> Just $ LogHandle StdErr
    _ -> Just $ LogHandle $ Path path
  optionName = return "log"
  optionHelp = return "log destination: (stderr), stdout, PATH"



-- | Log Noise Level.
data LogNoiseLevel
  = NoisyLog
  | SilentLog
  deriving Typeable

instance TO.IsOption LogNoiseLevel where
  defaultValue = NoisyLog
  parseValue str = case str of
    "noisy" -> Just NoisyLog
    "silent" -> Just SilentLog
    _ -> Nothing
  optionName = return "verbosity"
  optionHelp = return "log verbosity: (noisy), silent"



-- | Assertion log location.
newtype AssertionLogHandle
  = AssertionLogHandle { theAssertionLogHandle :: FileHandle }
  deriving Typeable

instance TO.IsOption AssertionLogHandle where
  defaultValue = AssertionLogHandle StdOut
  parseValue path = case path of
    "stdout" -> Just $ AssertionLogHandle StdOut
    "stderr" -> Just $ AssertionLogHandle StdErr
    _ -> Just $ AssertionLogHandle $ Path path
  optionName = return "assertion-log"
  optionHelp = return "assertion log destination: (stdout), stderr, PATH"



-- | Console in location. Used to mock stdin for testing.
newtype ConsoleInHandle
  = ConsoleInHandle { theConsoleInHandle :: FileHandle }
  deriving Typeable

instance TO.IsOption ConsoleInHandle where
  defaultValue = ConsoleInHandle StdIn
  parseValue path = case path of
    "stdin" -> Just $ ConsoleInHandle StdIn
    _ -> Just $ ConsoleInHandle $ Path path
  optionName = return "console-in"
  optionHelp = return "console input: (stdin), PATH"



-- | Console out location. Used to mock stdout for testing.
newtype ConsoleOutHandle
  = ConsoleOutHandle { theConsoleOutHandle :: FileHandle }
  deriving Typeable

instance TO.IsOption ConsoleOutHandle where
  defaultValue = ConsoleOutHandle StdOut
  parseValue path = case path of
    "stdout" -> Just $ ConsoleOutHandle StdOut
    "stderr" -> Just $ ConsoleOutHandle StdErr
    _ -> Just $ ConsoleOutHandle $ Path path
  optionName = return "console-out"
  optionHelp = return "console output: (stdout), stderr, PATH"



-- | Named deployment environment.
newtype Deployment
  = Deployment { theDeployment :: DeploymentTier }
  deriving (Eq, Typeable)

data DeploymentTier
  = DEV
  | TEST
  | PROD
  deriving (Eq, Show, Typeable)

instance TO.IsOption Deployment where
  defaultValue = Deployment DEV
  parseValue str = case str of
    "dev" -> Just $ Deployment DEV
    "test" -> Just $ Deployment TEST
    "prod" -> Just $ Deployment PROD
    _ -> Nothing
  optionName = return "deploy"
  optionHelp = return "deployment environment: (dev), test, prod"



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
  deriving (Eq, Typeable)



-- | Set local options if the @Driver@ option is a given value.
ifDriverIs :: DriverName -> (TT.TestTree -> TT.TestTree) -> TT.TestTree -> TT.TestTree
ifDriverIs driver f tree = T.askOption checkDriver
  where
    checkDriver :: Driver -> TT.TestTree
    checkDriver (Driver d) = if d == driver
      then f tree
      else tree



-- | Set local options if the @Driver@ option is not a given value.
unlessDriverIs :: DriverName -> (TT.TestTree -> TT.TestTree) -> TT.TestTree -> TT.TestTree
unlessDriverIs driver f tree = T.askOption checkDriver
  where
    checkDriver :: Driver -> TT.TestTree
    checkDriver (Driver d) = if d /= driver
      then f tree
      else tree



-- | Set local options if the @Deployment@ option is a given value.
ifTierIs :: DeploymentTier -> (TT.TestTree -> TT.TestTree) -> TT.TestTree -> TT.TestTree
ifTierIs tier f tree = T.askOption checkDeployment
  where
    checkDeployment :: Deployment -> TT.TestTree
    checkDeployment (Deployment t) = if t == tier
      then f tree
      else tree



-- | Set local options if the @Deployment@ option is not a given value.
unlessTierIs :: DeploymentTier -> (TT.TestTree -> TT.TestTree) -> TT.TestTree -> TT.TestTree
unlessTierIs tier f tree = T.askOption checkDeployment
  where
    checkDeployment :: Deployment -> TT.TestTree
    checkDeployment (Deployment t) = if t /= tier
      then f tree
      else tree

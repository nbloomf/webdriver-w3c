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
    defaultWebDriverMain

  -- * Test Case Constructors
  , testCase
  , testCaseWithSetup

  -- * Branching
  , ifDriverIs
  , ifTierIs
  , unlessDriverIs
  , unlessTierIs

  -- * Options
  , Driver(..)
  , DriverName(..)
  , SecretsPath(..)
  , Deployment(..)
  , DeploymentTier(..)
  , BrowserPath(..)
  , ApiResponseFormat(..)
  , WebDriverApiVersion(..)
  , LogHandle(..)
  , TestDelay(..)
  , LogNoiseLevel(..)
  , AssertionLogHandle(..)
  , ConsoleInHandle(..)
  , ConsoleOutHandle(..)
  , RemoteEndRef(..)
  , FileHandle(..)
  , Headless(..)

  , module Test.Tasty.WebDriver.Config
  ) where


import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Data.Typeable
  ( Typeable, Proxy(Proxy) )
import Data.List
  ( unlines )
import System.IO
  ( Handle, stdout, stderr, stdin, openFile, IOMode(..) )
import Data.IORef
  ( IORef, newIORef, atomicModifyIORef' )
import System.Exit
  ( exitFailure )
import Control.Concurrent
  ( threadDelay )
import Text.Read
  ( readMaybe )
import qualified Data.Map.Strict as MS
import qualified Test.Tasty as T
import qualified Test.Tasty.Providers as TT
import qualified Test.Tasty.Options as TO
import qualified Test.Tasty.ExpectedFailure as TE
import qualified System.Environment as SE
  ( getEnv, getArgs, lookupEnv )


import Web.Api.Http
import Web.Api.WebDriver

import Test.Tasty.WebDriver.Config


data WebDriverTest m = WebDriverTest
  { _test_name :: Maybe String
  , _test_session :: (WebDriver m ())
  } deriving Typeable

instance (Effectful m, Typeable m) => TT.IsTest (WebDriverTest m) where

  testOptions = return
    [ TO.Option (Proxy :: Proxy Driver)
    , TO.Option (Proxy :: Proxy Headless)
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
    , TO.Option (Proxy :: Proxy TestDelay)
    , TO.Option (Proxy :: Proxy RemoteEndRef)
    , TO.Option (Proxy :: Proxy RemoteEndOpt)
    ]

  run opts WebDriverTest{..} _ = do
    let
      Driver driver = TO.lookupOption opts
      Headless headless = TO.lookupOption opts
      ApiResponseFormat format = TO.lookupOption opts
      WebDriverApiVersion version = TO.lookupOption opts
      LogHandle log = TO.lookupOption opts
      logNoiseLevel = TO.lookupOption opts
      AssertionLogHandle alog = TO.lookupOption opts
      ConsoleInHandle cin = TO.lookupOption opts
      TestDelay delay = TO.lookupOption opts
      ConsoleOutHandle cout = TO.lookupOption opts
      SecretsPath secrets = TO.lookupOption opts
      BrowserPath browserPath = TO.lookupOption opts
      RemoteEndRef remotes = TO.lookupOption opts

    logHandle <- writeModeHandle log
    alogHandle <- writeModeHandle alog
    cinHandle <- readModeHandle cin
    coutHandle <- writeModeHandle cout

    secretsPath <- case secrets of
      "" -> fmap (++ "/.webdriver/secrets") $ SE.getEnv "HOME"
      spath -> return spath

    remotesRef <- case remotes of
      Just ref -> return ref
      Nothing -> do
        putStrLn "Error: no remote ends specified."
        exitFailure

    remote <- acquireRemoteEnd delay remotesRef driver

    let
      title = case _test_name of
        Nothing -> return ()
        Just str -> comment str

      logNoise = case logNoiseLevel of
        NoisyLog -> noisyLog
        SilentLog -> silentLog

      config =
        setEnvironment
          ( setLogHandle logHandle
          . setLogVerbosity logNoise
          . setAssertionLogHandle alogHandle
          . setConsoleInHandle cinHandle
          . setConsoleOutHandle coutHandle
          . setClientEnvironment
              ( setRemoteHostname (remoteEndHost remote)
              . setRemotePort (remoteEndPort remote)
              . setRemotePath (remoteEndPath remote)
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

    releaseRemoteEnd remotesRef driver remote

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
  optionName = return "wd-driver"
  optionHelp = return "remote end name: (geckodriver), chromedriver"



-- | Run in headless mode.
newtype Headless
  = Headless { theHeadless :: Bool }
  deriving Typeable

instance TO.IsOption Headless where
  defaultValue = Headless False
  parseValue = fmap Headless . TO.safeReadBool
  optionName = return "wd-headless"
  optionHelp = return "run in headless mode: (false), true"



-- | Path where secrets are stored.
newtype SecretsPath
  = SecretsPath { theSecretsPath :: FilePath }
  deriving Typeable

instance TO.IsOption SecretsPath where
  defaultValue = SecretsPath ""
  parseValue path = Just $ SecretsPath path
  optionName = return "wd-secrets"
  optionHelp = return "secrets path: (~/.webdriver/secrets), PATH"



-- | Path to browser binary.
newtype BrowserPath
  = BrowserPath { theBrowserPath :: Maybe FilePath }
  deriving Typeable

instance TO.IsOption BrowserPath where
  defaultValue = BrowserPath Nothing
  parseValue path = Just $ BrowserPath $ Just path
  optionName = return "wd-browserpath"
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
  optionName = return "wd-response-format"
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
  optionName = return "wd-api-version"
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
  optionName = return "wd-log"
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
  optionName = return "wd-verbosity"
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
  optionName = return "wd-assertion-log"
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
  optionName = return "wd-console-in"
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
  optionName = return "wd-console-out"
  optionHelp = return "console output: (stdout), stderr, PATH"



-- | Delay between test attempts.
newtype TestDelay = TestDelay
  { theTestDelay :: Int
  } deriving (Eq, Show, Typeable)

instance TO.IsOption TestDelay where
  defaultValue = TestDelay 500000
  parseValue = fmap TestDelay . readMaybe
  optionName = return "wd-delay"
  optionHelp = return "delay between test attempts in ms: (500000), INT"



-- | Named deployment environment.
newtype Deployment
  = Deployment { theDeployment :: DeploymentTier }
  deriving (Eq, Typeable)

-- | Representation of the deployment environment.
data DeploymentTier
  = DEV -- ^ Local environment
  | TEST -- ^ CI server (for testing the library)
  | PROD
  deriving (Eq, Show, Typeable)

instance TO.IsOption Deployment where
  defaultValue = Deployment DEV
  parseValue str = case str of
    "dev" -> Just $ Deployment DEV
    "test" -> Just $ Deployment TEST
    "prod" -> Just $ Deployment PROD
    _ -> Nothing
  optionName = return "wd-deploy"
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



-- | Mutable remote end pool
newtype RemoteEndRef = RemoteEndRef
  { theRemoteEndRef :: Maybe (IORef RemoteEndPool)
  } deriving (Typeable)

instance TO.IsOption RemoteEndRef where
  defaultValue = RemoteEndRef Nothing
  parseValue _ = Just $ RemoteEndRef Nothing
  optionName = return "wd-remote-ends-config"
  optionHelp = return "path to remote end config"



data RemoteEndOpt = RemoteEndOpt
  deriving Typeable

instance TO.IsOption RemoteEndOpt where
  defaultValue = RemoteEndOpt
  parseValue _ = Just RemoteEndOpt
  optionName = return "wd-remote-ends"
  optionHelp = return "remote end uris"



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



defaultWebDriverMain :: TT.TestTree -> IO ()
defaultWebDriverMain tree = do

  deploy <- determineDeploymentTier
  pool <- getRemoteEndRef

  T.defaultMain
    $ T.localOption (Deployment deploy)
    $ T.localOption (RemoteEndRef $ Just pool)
    $ tree


determineDeploymentTier :: IO DeploymentTier
determineDeploymentTier = do
  putStrLn "Determining deployment environment..."
  deploy <- do
    var <- SE.lookupEnv "CI"
    case var of
      Just "true" -> return TEST
      _ -> return DEV
  putStrLn $ "Deployment environment is " ++ show deploy
  return deploy


getRemoteEndRef :: IO (IORef RemoteEndPool)
getRemoteEndRef = do
  configPool <- getRemoteEndConfigPath
  optionPool <- getRemoteEndOptionString
  case (configPool, optionPool) of
    (Just x,  Just y)  -> newIORef $ combineRemoteEndPools x y
    (Nothing, Just y)  -> newIORef y
    (Just x,  Nothing) -> newIORef x
    (Nothing, Nothing) -> newIORef emptyRemoteEndPool


getRemoteEndConfigPath :: IO (Maybe RemoteEndPool)
getRemoteEndConfigPath = do
  args <- SE.getArgs
  let
    foo :: [String] -> Maybe (Maybe String)
    foo as = case as of
      ("--wd-remote-ends-config":('-':_):_) -> Nothing
      ("--wd-remote-ends-config":path:_) -> Just $ Just path
      (_:y:xs) -> foo (y:xs)
      _ -> Just Nothing
  case foo args of
    Just Nothing -> return Nothing
    Just (Just path) -> do
      str <- readFile path
      case parseRemoteEndConfig str of
        Left err -> do
          putStrLn err
          exitFailure
        Right x -> return (Just x)
    Nothing -> do
      putStrLn "option --wd-remote-ends-config missing required path argument"
      exitFailure


getRemoteEndOptionString :: IO (Maybe RemoteEndPool)
getRemoteEndOptionString = do
  args <- SE.getArgs
  let
    foo :: [String] -> Maybe (Maybe String)
    foo as = case as of
      ("--wd-remote-ends":('-':_):_) -> Nothing
      ("--wd-remote-ends":path:_) -> Just $ Just path
      (_:y:xs) -> foo (y:xs)
      _ -> Just Nothing
  case foo args of
    Just Nothing -> return Nothing
    Just (Just str) -> do
      case parseRemoteEndOption str of
        Left err -> do
          putStrLn err
          exitFailure
        Right x -> return (Just x)
    Nothing -> do
      putStrLn "option --wd-remote-ends missing required argument"
      exitFailure


acquireRemoteEnd :: Int -> IORef RemoteEndPool -> DriverName -> IO RemoteEnd
acquireRemoteEnd delay ref driver = do
  let
    update :: RemoteEndPool -> (RemoteEndPool, Maybe (Maybe RemoteEnd))
    update = getRemoteEndForDriver driver

  result <- atomicModifyIORef' ref update

  case result of
    Nothing -> do
      putStrLn $ "Error: no remotes defined for " ++ show driver
      exitFailure
    Just Nothing -> do
      threadDelay delay
      acquireRemoteEnd delay ref driver
    Just (Just x) -> return x


releaseRemoteEnd :: IORef RemoteEndPool -> DriverName -> RemoteEnd -> IO ()
releaseRemoteEnd ref driver remote = do
  let
    update :: RemoteEndPool -> (RemoteEndPool, ())
    update pool = (addRemoteEndForDriver driver remote pool, ())

  atomicModifyIORef' ref update

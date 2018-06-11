{- |
Module      : Test.Tasty.WebDriver
Description : WebDriver integration with the Tasty test framework.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE DeriveDataTypeable, RecordWildCards, Rank2Types #-}
module Test.Tasty.WebDriver (
    defaultWebDriverMain

  -- * Test Case Constructors
  , testCase
  , testCaseM
  , testCaseWithSetup
  , testCaseWithSetupM

  -- * Branching
  , ifDriverIs
  , ifTierIs
  , ifHeadless
  , unlessDriverIs
  , unlessTierIs
  , unlessHeadless

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
  , NumRetries(..)
  , LogNoiseLevel(..)
  , ConsoleInHandle(..)
  , ConsoleOutHandle(..)
  , RemoteEndRef(..)
  , FileHandle(..)
  , Headless(..)
  , LogColors(..)
  , GeckodriverLog(..)

  , module Test.Tasty.WebDriver.Config
  ) where



import Control.Concurrent
  ( threadDelay )
import Control.Concurrent.MVar
  ( MVar, newMVar, withMVar )
import Control.Concurrent.STM
  
import Control.Lens
  ((.~), (&))
import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import qualified Data.ByteString.Lazy.Char8 as BS
  ( pack )
import qualified Data.Digest.Pure.SHA as SHA
  ( showDigest, sha1 )
import Data.IORef
  ( IORef, newIORef, atomicModifyIORef' )
import Data.List
  ( unlines )
import Data.Maybe
  ( fromMaybe )
import Data.Time.Clock.System
  ( getSystemTime )
import Data.Typeable
  ( Typeable, Proxy(Proxy) )
import Network.HTTP.Client
  ( defaultManagerSettings, managerResponseTimeout, ResponseTimeout(..)
  , responseTimeoutNone )
import qualified Network.Wreq as Wreq
  ( defaults, manager )
import qualified System.Environment as SE
  ( getEnv, getArgs, lookupEnv )
import System.Exit
  ( exitFailure )
import System.IO
  ( Handle, stdout, stderr, stdin, openFile, IOMode(..) )
import Text.Read
  ( readMaybe )

import qualified Data.Map.Strict as MS
import qualified Test.Tasty as T
import qualified Test.Tasty.Providers as TT
import qualified Test.Tasty.Options as TO
import qualified Test.Tasty.ExpectedFailure as TE

import Web.Api.WebDriver
import Test.Tasty.WebDriver.Config



data WebDriverTest m = WebDriverTest
  { wdTestName :: String
  , wdTestSession :: WebDriver ()
  , wdEval :: forall a. P WDAct a -> m a
  , wdToIO :: forall a. m a -> IO a
  }

instance (Monad m, Typeable m) => TT.IsTest (WebDriverTest m) where
  testOptions = return
    [ TO.Option (Proxy :: Proxy Driver)
    , TO.Option (Proxy :: Proxy Headless)
    , TO.Option (Proxy :: Proxy ApiResponseFormat)
    , TO.Option (Proxy :: Proxy WebDriverApiVersion)
    , TO.Option (Proxy :: Proxy LogHandle)
    , TO.Option (Proxy :: Proxy LogNoiseLevel)
    , TO.Option (Proxy :: Proxy ConsoleInHandle)
    , TO.Option (Proxy :: Proxy ConsoleOutHandle)
    , TO.Option (Proxy :: Proxy Deployment)
    , TO.Option (Proxy :: Proxy SecretsPath)
    , TO.Option (Proxy :: Proxy BrowserPath)
    , TO.Option (Proxy :: Proxy TestDelay)
    , TO.Option (Proxy :: Proxy RemoteEndRef)
    , TO.Option (Proxy :: Proxy RemoteEndOpt)
    , TO.Option (Proxy :: Proxy NumRetries)
    , TO.Option (Proxy :: Proxy LogColors)
    , TO.Option (Proxy :: Proxy GeckodriverLog)
    ]

  run opts WebDriverTest{..} _ = do
    let
      Driver driver = TO.lookupOption opts
      Headless headless = TO.lookupOption opts
      ApiResponseFormat format = TO.lookupOption opts
      WebDriverApiVersion version = TO.lookupOption opts
      LogHandle log = TO.lookupOption opts
      logNoiseLevel = TO.lookupOption opts
      ConsoleInHandle cin = TO.lookupOption opts
      TestDelay delay = TO.lookupOption opts
      ConsoleOutHandle cout = TO.lookupOption opts
      SecretsPath secrets = TO.lookupOption opts
      BrowserPath browserPath = TO.lookupOption opts
      RemoteEndRef remotes = TO.lookupOption opts
      NumRetries numRetries = TO.lookupOption opts
      LogPrinterLock (Just logLock) = TO.lookupOption opts
      LogColors logColors = TO.lookupOption opts
      GeckodriverLog geckoLogLevel = TO.lookupOption opts

    let
      title = comment wdTestName

      attemptLabel k = comment $ "Attempt #" ++ show k

      logNoise = case logNoiseLevel of
        NoisyLog -> False
        SilentLog -> True

      caps = case driver of
        Geckodriver -> emptyCapabilities
          { _browserName = Just Firefox
          , _firefoxOptions = Just $ defaultFirefoxOptions
              { _firefoxBinary = browserPath
              , _firefoxArgs = if headless then Just ["-headless"] else Nothing
              , _firefoxLog = Just $ FirefoxLog
                  { _firefoxLogLevel = Just geckoLogLevel
                  }
              }
          }

        Chromedriver -> emptyCapabilities
          { _browserName = Just Chrome
          , _chromeOptions = Just $ defaultChromeOptions
              { _chromeBinary = browserPath
              , _chromeArgs = if headless then Just ["--headless"] else Nothing
              }
          }

    logHandle <- writeModeHandle log
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

    let
      attempt :: Int -> IO TT.Result
      attempt attemptNumber = do

        remote <- acquireRemoteEnd remotesRef delay driver

        let
          uid = digest wdTestName ++ "-" ++ show attemptNumber
            where
              digest :: (Show a) => a -> String
              digest = take 8 . SHA.showDigest . SHA.sha1 . BS.pack . show

          config = WDConfig
            { _evaluator = wdEval
            , _initialState = S
              { _httpOptions = Wreq.defaults
                  & Wreq.manager .~ Left (defaultManagerSettings 
                    { managerResponseTimeout = responseTimeoutNone } )
              , _httpSession = Nothing
              , _userState = WDState
                { _sessionId = Nothing
                }
              }
            , _environment = R
              { _logHandle = logHandle
              , _logLock = logLock
              , _uid = uid
              , _logOptions = LogOptions
                { _logColor = logColors
                , _logJson = True
                , _logSilent = logNoise
                , _printUserError = printWDError
                , _printUserLog = printWDLog
                }
              , _httpErrorInject = promoteHttpResponseError
              , _userEnv = WDEnv
                { _remoteHostname = remoteEndHost remote
                , _remotePort = remoteEndPort remote
                , _remotePath = remoteEndPath remote
                , _responseFormat = format
                , _apiVersion = version
                , _secretsPath = secretsPath
                , _vars = MS.fromList []
                , _stdout = coutHandle
                , _stdin = cinHandle
                }
              }
            }

        (result, finalState, theLog) <- wdToIO $ execWebDriver config $
            title >> attemptLabel attemptNumber >> runIsolated caps wdTestSession

        atomically $ releaseRemoteEnd remotesRef driver remote

        let assertions = getAssertions $ logEntries theLog

        case result of
          Right _ ->
            return $ webDriverAssertionsToResult $ summarize assertions
          Left err -> if attemptNumber >= numRetries
            then return $ TT.testFailed $
              "Unhandled error: " ++ printE (printWDError True) err
            else attempt (attemptNumber + 1)

    attempt 1



webDriverAssertionsToResult :: AssertionSummary -> TT.Result
webDriverAssertionsToResult x =
  if numFailures x > 0
    then TT.testFailed $ unlines $ map printAssertion $ failures x
    else TT.testPassed $ show (numSuccesses x) ++ " assertion(s)"



-- | Simple WebDriver test case.
testCase
  :: TT.TestName
  -> WebDriver ()
  -> TT.TestTree
testCase name test =
  testCaseWithSetup name (return ()) return (\_ -> test)



-- | Simple WebDriver test case with a custom effect evaluator.
testCaseM
  :: (Monad m, Typeable m)
  => TT.TestName
  -> (forall a. P WDAct a -> m a)
  -> (forall a. m a -> IO a)
  -> WebDriver ()
  -> TT.TestTree
testCaseM name eval toIO test =
  testCaseWithSetupM name eval toIO (return ()) return (\_ -> test)



-- | WebDriver test case with additional setup and teardown phases -- setup runs before the test (for e.g. logging in) and teardown runs after the test (for e.g. deleting temp files).
testCaseWithSetup
  :: TT.TestName
  -> WebDriver u -- ^ Setup
  -> (v -> WebDriver ()) -- ^ Teardown
  -> (u -> WebDriver v) -- ^ The test
  -> TT.TestTree
testCaseWithSetup name =
  testCaseWithSetupM name (evalIO evalWDAct) id



-- | WebDriver test case with additional setup and teardown phases and a custom effect evaluator. Setup runs before the test (for logging in, say) and teardown runs after the test (for deleting temp files, say). 
testCaseWithSetupM
  :: (Monad m, Typeable m)
  => TT.TestName
  -> (forall a. P WDAct a -> m a) -- ^ Evaluator
  -> (forall a. m a -> IO a) -- ^ Conversion to `IO`.
  -> WebDriver u -- ^ Setup
  -> (v -> WebDriver ()) -- ^ Teardown
  -> (u -> WebDriver v) -- ^ Test
  -> TT.TestTree
testCaseWithSetupM name eval toIO setup teardown test =
  TT.singleTest name WebDriverTest
    { wdTestName = name
    , wdTestSession = setup >>= test >>= teardown
    , wdEval = eval
    , wdToIO = toIO
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



newtype LogColors
  = LogColors { theLogColors :: Bool }
  deriving Typeable

instance TO.IsOption LogColors where
  defaultValue = LogColors True
  parseValue = fmap LogColors . TO.safeReadBool
  optionName = return "wd-color"
  optionHelp = return "colored logs: (true), false"



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



newtype GeckodriverLog
  = GeckodriverLog { theGeckodriverLog :: LogLevel }
  deriving Typeable

instance TO.IsOption GeckodriverLog where
  defaultValue = GeckodriverLog LogInfo
  parseValue level = case level of
    "trace" -> Just $ GeckodriverLog LogTrace
    "debug" -> Just $ GeckodriverLog LogDebug
    "config" -> Just $ GeckodriverLog LogConfig
    "info" -> Just $ GeckodriverLog LogInfo
    "warn" -> Just $ GeckodriverLog LogWarn
    "error" -> Just $ GeckodriverLog LogError
    "fatal" -> Just $ GeckodriverLog LogFatal
    _ -> Nothing
  optionName = return "wd-geckodriver-log"
  optionHelp = return "log level passed to geckodriver: trace, debug, config, info, warn, error, fatal"



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



newtype LogPrinterLock = LogPrinterLock
  { theLogPrinterLock :: Maybe (MVar ())
  } deriving Typeable

instance TO.IsOption LogPrinterLock where
  defaultValue = LogPrinterLock Nothing
  parseValue = error "LogPrinterLock is an internal option."
  optionName = error "LogPrinterLock is an internal option."
  optionHelp = error "LogPrinterLock is an internal option."



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



-- | Max number of retries.
newtype NumRetries
  = NumRetries { theNumRetries :: Int }
  deriving Typeable

instance TO.IsOption NumRetries where
  defaultValue = NumRetries 1
  parseValue = fmap NumRetries . readMaybe
  optionName = return "wd-num-retries"
  optionHelp = return "number of times to retry a failed test"



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
  defaultValue = TestDelay 800000
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
  { theRemoteEndRef :: Maybe (TVar RemoteEndPool)
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



-- | Set local options if `Headless` is true.
ifHeadless :: (TT.TestTree -> TT.TestTree) -> TT.TestTree -> TT.TestTree
ifHeadless f tree = T.askOption checkHeadless
  where
    checkHeadless :: Headless -> TT.TestTree
    checkHeadless (Headless p) = (if p then f else id) tree

-- | Set local options if `Headless` is false.
unlessHeadless :: (TT.TestTree -> TT.TestTree) -> TT.TestTree -> TT.TestTree
unlessHeadless f tree = T.askOption checkHeadless
  where
    checkHeadless :: Headless -> TT.TestTree
    checkHeadless (Headless p) = (if p then id else f) tree



-- | Run a tree of webdriver tests. Thin wrapper around tasty's @defaultMain@ that attempts to determine the deployment tier and interprets remote end config command line options.
defaultWebDriverMain :: TT.TestTree -> IO ()
defaultWebDriverMain tree = do
  logLock <- newMVar ()
  deploy <- determineDeploymentTier
  pool <- getRemoteEndRef

  T.defaultMain
    . T.localOption (Deployment deploy)
    . T.localOption (RemoteEndRef $ Just pool)
    . T.localOption (LogPrinterLock $ Just logLock)
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


getRemoteEndRef :: IO (TVar RemoteEndPool)
getRemoteEndRef = do
  configPool <- fromMaybe mempty <$> getRemoteEndConfigPath
  optionPool <- fromMaybe mempty <$> getRemoteEndOptionString
  newTVarIO $ mappend configPool optionPool


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
    Just (Just str) ->
      case parseRemoteEndOption str of
        Left err -> do
          putStrLn err
          exitFailure
        Right x -> return (Just x)
    Nothing -> do
      putStrLn "option --wd-remote-ends missing required argument"
      exitFailure

acquireRemoteEnd :: TVar RemoteEndPool -> Int -> DriverName -> IO RemoteEnd
acquireRemoteEnd var delay driver = do
  result <- atomically $ acquireRemoteEndSTM var driver
  case result of
    Nothing -> do
      putStrLn $ "Error: no remotes defined for " ++ show driver
      exitFailure
    Just Nothing -> do
      threadDelay delay
      acquireRemoteEnd var delay driver
    Just (Just x) -> return x

acquireRemoteEndSTM
  :: TVar RemoteEndPool -> DriverName -> STM (Maybe (Maybe RemoteEnd))
acquireRemoteEndSTM var driver = do
  pool <- readTVar var
  let (newPool, result) = getRemoteEndForDriver driver pool
  writeTVar var newPool
  return result

releaseRemoteEnd :: TVar RemoteEndPool -> DriverName -> RemoteEnd -> STM ()
releaseRemoteEnd var driver remote =
  modifyTVar' var $ addRemoteEndForDriver driver remote

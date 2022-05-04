{- |
Module      : Test.Tasty.WebDriver
Description : WebDriverT integration with the Tasty test framework.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

Tasty integration for `WebDriverT` tests.
-}

{-# LANGUAGE DeriveDataTypeable, RecordWildCards, Rank2Types, OverloadedStrings #-}
module Test.Tasty.WebDriver (
    defaultWebDriverMain

  -- * Test Case Constructors
  , testCase
  , testCaseM
  , testCaseT
  , testCaseTM
  , testCaseWithSetup
  , testCaseWithSetupM
  , testCaseWithSetupT
  , testCaseWithSetupTM

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
  , DataPath(..)
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
  , Headless(..)
  , LogColors(..)
  , GeckodriverLog(..)
  , PrivateMode(..)

  , module Test.Tasty.WebDriver.Config
  ) where



import Control.Monad.Trans.Class
  ( MonadTrans(..) )
import Control.Monad.Trans.Identity
  ( IdentityT(..) )
import Data.Typeable
  ( Typeable, Proxy(Proxy) )
import System.IO
  ( Handle, stdout, stderr, stdin, openFile, IOMode(..), hClose )
import Control.Concurrent
  ( threadDelay )
import Control.Concurrent.MVar
  ( newMVar )
import Control.Concurrent.STM
  
import Control.Lens
  ((.~), (&))
import qualified Data.ByteString.Lazy.Char8 as BS
  ( pack )
import qualified Data.Digest.Pure.SHA as SHA
  ( showDigest, sha1 )
import Data.Maybe
  ( fromMaybe, catMaybes )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.String
import Network.HTTP.Client
  ( defaultManagerSettings, managerResponseTimeout
  , responseTimeoutNone )
import qualified Network.Wreq as Wreq
  ( defaults, manager )
import qualified System.Environment as SE
  ( getEnv, setEnv, getArgs, lookupEnv )
import System.Exit
  ( exitFailure )
import Text.Read
  ( readMaybe )

import qualified Data.Map.Strict as MS
import Data.Text (Text)
import qualified Test.Tasty as T
import qualified Test.Tasty.Providers as TT
import qualified Test.Tasty.Options as TO

import Web.Api.WebDriver
import Test.Tasty.WebDriver.Config



_OPT_LOG_HANDLE :: (IsString t) => t
_OPT_LOG_HANDLE = "wd-log"

_OPT_CONSOLE_OUT :: (IsString t) => t
_OPT_CONSOLE_OUT = "wd-console-out"

_OPT_CONSOLE_IN :: (IsString t) => t
_OPT_CONSOLE_IN = "wd-console-in"

_OPT_COLOR :: (IsString t) => t
_OPT_COLOR = "wd-color"

_OPT_HEADLESS :: (IsString t) => t
_OPT_HEADLESS = "wd-headless"

_OPT_DRIVER :: (IsString t) => t
_OPT_DRIVER = "wd-driver"

_OPT_GECKODRIVER_LOG :: (IsString t) => t
_OPT_GECKODRIVER_LOG = "wd-geckodriver-log"

_OPT_BROWSERPATH :: (IsString t) => t
_OPT_BROWSERPATH = "wd-browserpath"

_OPT_DEPLOYMENT :: (IsString t) => t
_OPT_DEPLOYMENT = "wd-deploy"

_OPT_REMOTE_ENDS :: (IsString t) => t
_OPT_REMOTE_ENDS = "wd-remote-ends"

_OPT_DATA_PATH :: (IsString t) => t
_OPT_DATA_PATH = "wd-data-path"

_OPT_RESPONSE_FORMAT :: (IsString t) => t
_OPT_RESPONSE_FORMAT = "wd-response-format"

_OPT_API_VERSION :: (IsString t) => t
_OPT_API_VERSION = "wd-api-version"

_OPT_VERBOSITY :: (IsString t) => t
_OPT_VERBOSITY = "wd-verbosity"

_OPT_NUM_RETRIES :: (IsString t) => t
_OPT_NUM_RETRIES = "wd-num-retries"

_OPT_DELAY :: (IsString t) => t
_OPT_DELAY = "wd-delay"

_OPT_REMOTE_ENDS_CONFIG :: (IsString t) => t
_OPT_REMOTE_ENDS_CONFIG = "wd-remote-ends-config"

_OPT_PRIVATE_MODE :: (IsString t) => t
_OPT_PRIVATE_MODE = "wd-private-mode"



data WebDriverTest t eff = WebDriverTest
  { wdTestName :: Text
  , wdTestSession :: WebDriverTT t eff ()
  , wdEval :: forall a. P WDAct a -> eff a
  , wdToIO :: forall a. t eff a -> IO a
  }



instance
  (Monad eff, Monad (t eff), MonadTrans t, Typeable eff, Typeable t)
    => TT.IsTest (WebDriverTest t eff) where
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
    , TO.Option (Proxy :: Proxy DataPath)
    , TO.Option (Proxy :: Proxy BrowserPath)
    , TO.Option (Proxy :: Proxy TestDelay)
    , TO.Option (Proxy :: Proxy RemoteEndRef)
    , TO.Option (Proxy :: Proxy RemoteEndOpt)
    , TO.Option (Proxy :: Proxy NumRetries)
    , TO.Option (Proxy :: Proxy LogColors)
    , TO.Option (Proxy :: Proxy GeckodriverLog)
    , TO.Option (Proxy :: Proxy PrivateMode)
    ]

  run opts WebDriverTest{..} _ = do
    let
      Driver driver = TO.lookupOption opts
      Headless headless = TO.lookupOption opts
      ApiResponseFormat format = TO.lookupOption opts
      WebDriverApiVersion version = TO.lookupOption opts
      LogHandle logHandle = TO.lookupOption opts
      logNoiseLevel = TO.lookupOption opts
      ConsoleInHandle cinHandle = TO.lookupOption opts
      TestDelay delay = TO.lookupOption opts
      ConsoleOutHandle coutHandle = TO.lookupOption opts
      DataPath datas = TO.lookupOption opts
      BrowserPath browserPath = TO.lookupOption opts
      RemoteEndRef remotes = TO.lookupOption opts
      NumRetries numRetries = TO.lookupOption opts
      LogColors logColors = TO.lookupOption opts
      GeckodriverLog geckoLogLevel = TO.lookupOption opts
      PrivateMode privateMode = TO.lookupOption opts

    let
      title = comment wdTestName

      attemptLabel k = comment $ "Attempt #" <> Text.pack (show k)

      logNoise = case logNoiseLevel of
        NoisyLog -> False
        SilentLog -> True

      caps = case driver of
        Geckodriver -> emptyCapabilities
          { _browserName = Just Firefox
          , _firefoxOptions = Just defaultFirefoxOptions
            { _firefoxBinary = browserPath
            , _firefoxArgs = Just $ catMaybes
              [ if headless then Just "-headless" else Nothing
              , if privateMode then Just "-private" else Nothing
              ]
            , _firefoxLog = Just FirefoxLog
              { _firefoxLogLevel = Just geckoLogLevel
              }
            }
          }

        Chromedriver -> emptyCapabilities
          { _browserName = Just Chrome
          , _chromeOptions = Just $ defaultChromeOptions
            { _chromeBinary = browserPath
            , _chromeArgs = Just $ catMaybes
              [ if headless then Just "--headless" else Nothing
              , if privateMode then Just "--incognito" else Nothing
              ]
            }
          }

    dataPath <- case datas of
      Nothing -> fmap (++ "/.webdriver") $ SE.getEnv "HOME"
      Just dpath -> return dpath

    remotesRef <- case remotes of
      Just ref -> return ref
      Nothing -> do
        putStrLn "Error: no remote ends specified."
        exitFailure

    logLock <- newMVar ()

    let
      attempt :: Int -> IO TT.Result
      attempt attemptNumber = do

        remote <- acquireRemoteEnd remotesRef delay driver

        let
          uid = digest wdTestName <> "-" <> Text.pack (show attemptNumber) <> " " <> Text.pack (show remote)
            where
              digest :: (Show a) => a -> Text
              digest = Text.pack . take 8 . SHA.showDigest . SHA.sha1 . BS.pack . show

          config = WDConfig
            { _evaluator = wdEval
            , _initialState = S
              { _httpOptions = Wreq.defaults
                  & Wreq.manager .~ Left (defaultManagerSettings 
                    { managerResponseTimeout = responseTimeoutNone } )
              , _httpSession = Nothing
              , _userState = WDState
                { _sessionId = Nothing
                , _breakpoints = BreakpointsOff
                }
              }
            , _environment = defaultWebDriverEnvironment
              { _logHandle = logHandle
              , _logLock = Just logLock
              , _uid = uid
              , _logOptions = defaultWebDriverLogOptions
                { _logColor = logColors
                , _logJson = True
                , _logHeaders = False
                , _logSilent = logNoise
                }
              , _env = WDEnv
                { _remoteHostname = remoteEndHost remote
                , _remotePort = remoteEndPort remote
                , _remotePath = remoteEndPath remote
                , _responseFormat = format
                , _apiVersion = version
                , _dataPath = dataPath
                , _stdout = coutHandle
                , _stdin = cinHandle
                }
              }
            }

        (result, summary) <- wdToIO $ debugWebDriverTT config $
            title >> attemptLabel attemptNumber >> runIsolated_ caps wdTestSession

        atomically $ releaseRemoteEnd remotesRef driver remote

        case result of
          Right _ ->
            return $ webDriverAssertionsToResult summary
          Left err -> if attemptNumber >= numRetries
            then return $ TT.testFailed $ Text.unpack $ "Unhandled error!\n" <> err
            else attempt (attemptNumber + 1)

    attempt 1



webDriverAssertionsToResult :: AssertionSummary -> TT.Result
webDriverAssertionsToResult x =
  if numFailures x > 0
    then TT.testFailed $ unlines $ map (Text.unpack . printAssertion) $ failures x
    else TT.testPassed $ show (numSuccesses x) ++ " assertion(s)"



-- | `WebDriver` test case with the default `IO` effect evaluator.
testCase
  :: TT.TestName
  -> WebDriverT IO () -- ^ The test
  -> TT.TestTree
testCase name test =
  testCaseWithSetup name (return ()) return (const test)


-- | `WebDriver` test case with a custom effect evaluator.
testCaseM
  :: (Monad eff, Typeable eff)
  => TT.TestName
  -> (forall a. P WDAct a -> eff a) -- ^ Evaluator
  -> (forall a. eff a -> IO a) -- ^ Conversion to `IO`
  -> WebDriverT eff ()
  -> TT.TestTree
testCaseM name eval toIO test =
  testCaseWithSetupM name eval toIO (return ()) return (const test)


-- | `WebDriverT` test case with the default `IO` effect evaluator.
testCaseT
  :: (Monad (t IO), MonadTrans t, Typeable t)
  => TT.TestName
  -> (forall a. t IO a -> IO a) -- ^ Conversion to `IO`
  -> WebDriverTT t IO () -- ^ The test
  -> TT.TestTree
testCaseT name toIO test =
  testCaseWithSetupT name toIO (return ()) return (const test)


-- | `WebDriverT` test case with a custom effect evaluator.
testCaseTM
  :: (Monad eff, Monad (t eff), MonadTrans t, Typeable eff, Typeable t)
  => TT.TestName
  -> (forall a. P WDAct a -> eff a) -- ^ Evaluator
  -> (forall a. t eff a -> IO a) -- ^ Conversion to `IO`.
  -> WebDriverTT t eff () -- ^ The test
  -> TT.TestTree
testCaseTM name eval toIO test =
  testCaseWithSetupTM name eval toIO (return ()) return (const test)


-- | `WebDriver` test case with additional setup and teardown phases using the default `IO` effect evaluator. Setup runs before the test (for e.g. logging in) and teardown runs after the test (for e.g. deleting temp files).
testCaseWithSetup
  :: TT.TestName
  -> WebDriverT IO u -- ^ Setup
  -> (v -> WebDriverT IO ()) -- ^ Teardown
  -> (u -> WebDriverT IO v) -- ^ The test
  -> TT.TestTree
testCaseWithSetup name =
  testCaseWithSetupM name (evalIO evalWDAct) id


-- | `WebDriver` test case with additional setup and teardown phases and a custom effect evaluator. Setup runs before the test (for e.g. logging in) and teardown runs after the test (for e.g. deleting temp files).
testCaseWithSetupM
  :: (Monad eff, Typeable eff)
  => TT.TestName
  -> (forall a. P WDAct a -> eff a) -- ^ Evaluator
  -> (forall a. eff a -> IO a) -- ^ Conversion to `IO`
  -> WebDriverT eff u -- ^ Setup
  -> (v -> WebDriverT eff ()) -- ^ Teardown
  -> (u -> WebDriverT eff v) -- ^ The test
  -> TT.TestTree
testCaseWithSetupM name eval toIO =
  testCaseWithSetupTM name eval (toIO . runIdentityT)


-- | `WebDriverT` test case with additional setup and teardown phases using the default `IO` effect evaluator. Setup runs before the test (for e.g. logging in) and teardown runs after the test (for e.g. deleting temp files).
testCaseWithSetupT
  :: (Monad (t IO), MonadTrans t, Typeable t)
  => TT.TestName
  -> (forall a. t IO a -> IO a) -- ^ Conversion to `IO`
  -> WebDriverTT t IO u -- ^ Setup
  -> (v -> WebDriverTT t IO ()) -- ^ Teardown
  -> (u -> WebDriverTT t IO v) -- ^ Test
  -> TT.TestTree
testCaseWithSetupT name =
  testCaseWithSetupTM name (evalIO evalWDAct)


-- | `WebDriverT` test case with additional setup and teardown phases and a custom effect evaluator. Setup runs before the test (for logging in, say) and teardown runs after the test (for deleting temp files, say). 
testCaseWithSetupTM
  :: (Monad eff, Monad (t eff), MonadTrans t, Typeable eff, Typeable t)
  => TT.TestName
  -> (forall a. P WDAct a -> eff a) -- ^ Evaluator
  -> (forall a. t eff a -> IO a) -- ^ Conversion to `IO`.
  -> WebDriverTT t eff u -- ^ Setup
  -> (v -> WebDriverTT t eff ()) -- ^ Teardown
  -> (u -> WebDriverTT t eff v) -- ^ Test
  -> TT.TestTree
testCaseWithSetupTM name eval toIO setup teardown test =
  TT.singleTest name WebDriverTest
    { wdTestName = Text.pack name
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
  optionName = return _OPT_DRIVER
  optionHelp = return "remote end name: (geckodriver), chromedriver"



-- | Governs whether logs are printed in color
newtype LogColors
  = LogColors { theLogColors :: Bool }
  deriving Typeable

instance TO.IsOption LogColors where
  defaultValue = LogColors True
  parseValue = fmap LogColors . TO.safeReadBool
  optionName = return _OPT_COLOR
  optionHelp = return "colored logs: (true), false"



-- | Run in headless mode.
newtype Headless
  = Headless { theHeadless :: Bool }
  deriving Typeable

instance TO.IsOption Headless where
  defaultValue = Headless False
  parseValue = fmap Headless . TO.safeReadBool
  optionName = return _OPT_HEADLESS
  optionHelp = return "run in headless mode: (false), true"



-- | Run in private mode.
newtype PrivateMode
  = PrivateMode { thePrivateMode :: Bool }
  deriving Typeable

instance TO.IsOption PrivateMode where
  defaultValue = PrivateMode False
  parseValue = fmap PrivateMode . TO.safeReadBool
  optionName = return _OPT_PRIVATE_MODE
  optionHelp = return "run in private mode: (false), true"



-- | Path where secrets are stored.
newtype DataPath
  = DataPath { theDataPath :: Maybe FilePath }
  deriving Typeable

instance TO.IsOption DataPath where
  defaultValue = DataPath Nothing
  parseValue path = Just $ DataPath $ Just path
  optionName = return _OPT_DATA_PATH
  optionHelp = return "data path: (~/.webdriver), PATH"



-- | Verbosity level passed to @geckodriver@
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
  optionName = return _OPT_GECKODRIVER_LOG
  optionHelp = return "log level passed to geckodriver: trace, debug, config, info, warn, error, fatal"



-- | Path to browser binary.
newtype BrowserPath
  = BrowserPath { theBrowserPath :: Maybe FilePath }
  deriving Typeable

instance TO.IsOption BrowserPath where
  defaultValue = BrowserPath Nothing
  parseValue path = Just $ BrowserPath $ Just path
  optionName = return _OPT_BROWSERPATH
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
  optionName = return _OPT_RESPONSE_FORMAT
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
  optionName = return _OPT_API_VERSION
  optionHelp = return "WebDriverT API version: (cr-2018-03-04)"



-- | Log location.
newtype LogHandle
  = LogHandle { theLogHandle :: Handle }
  deriving Typeable

instance TO.IsOption LogHandle where
  defaultValue = LogHandle stderr
  parseValue _ = Just $ LogHandle stderr
  optionName = return _OPT_LOG_HANDLE
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
  optionName = return _OPT_VERBOSITY
  optionHelp = return "log verbosity: (noisy), silent"



-- | Max number of retries.
newtype NumRetries
  = NumRetries { theNumRetries :: Int }
  deriving Typeable

instance TO.IsOption NumRetries where
  defaultValue = NumRetries 1
  parseValue = fmap NumRetries . readMaybe
  optionName = return _OPT_NUM_RETRIES
  optionHelp = return "number of times to retry a failed test"



-- | Console in location. Used to mock stdin for testing.
newtype ConsoleInHandle
  = ConsoleInHandle { theConsoleInHandle :: Handle }
  deriving Typeable

instance TO.IsOption ConsoleInHandle where
  defaultValue = ConsoleInHandle stdin
  parseValue _ = Just $ ConsoleInHandle stdin
  optionName = return _OPT_CONSOLE_IN
  optionHelp = return "console input: (stdin), PATH"



-- | Console out location. Used to mock stdout for testing.
newtype ConsoleOutHandle
  = ConsoleOutHandle { theConsoleOutHandle :: Handle }
  deriving Typeable

instance TO.IsOption ConsoleOutHandle where
  defaultValue = ConsoleOutHandle stdout
  parseValue _ = Just $ ConsoleOutHandle stdout
  optionName = return _OPT_CONSOLE_OUT
  optionHelp = return "console output: (stdout), stderr, PATH"



-- | Delay between test attempts.
newtype TestDelay = TestDelay
  { theTestDelay :: Int
  } deriving (Eq, Show, Typeable)

instance TO.IsOption TestDelay where
  defaultValue = TestDelay 800000
  parseValue = fmap TestDelay . readMaybe
  optionName = return _OPT_DELAY
  optionHelp = return "delay between test attempts in ms: (500000), INT"



-- | Named deployment environment.
newtype Deployment
  = Deployment { theDeployment :: DeploymentTier }
  deriving (Eq, Typeable)

-- | Representation of the deployment environment.
data DeploymentTier
  = DEV -- ^ Local environment
  | TEST -- ^ CI server (for testing the library)
  | PROD -- ^ "Production" -- e.g. testing a real site
  deriving (Eq, Show, Typeable)

instance TO.IsOption Deployment where
  defaultValue = Deployment DEV
  parseValue str = case str of
    "dev" -> Just $ Deployment DEV
    "test" -> Just $ Deployment TEST
    "prod" -> Just $ Deployment PROD
    _ -> Nothing
  optionName = return _OPT_DEPLOYMENT
  optionHelp = return "deployment environment: (dev), test, prod"



-- | Mutable remote end pool
newtype RemoteEndRef = RemoteEndRef
  { theRemoteEndRef :: Maybe (TVar RemoteEndPool)
  } deriving (Typeable)

instance TO.IsOption RemoteEndRef where
  defaultValue = RemoteEndRef Nothing
  parseValue _ = Just $ RemoteEndRef Nothing
  optionName = return _OPT_REMOTE_ENDS_CONFIG
  optionHelp = return "path to remote end config"



data RemoteEndOpt = RemoteEndOpt
  deriving Typeable

instance TO.IsOption RemoteEndOpt where
  defaultValue = RemoteEndOpt
  parseValue _ = Just RemoteEndOpt
  optionName = return _OPT_REMOTE_ENDS
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



-- | Run a tree of WebDriverT tests. Thin wrapper around tasty's @defaultMain@ that attempts to determine the deployment tier and interprets remote end config command line options.
defaultWebDriverMain :: TT.TestTree -> IO ()
defaultWebDriverMain tree = do
  pool <- getRemoteEndRef

  -- Determine the deployment tier
  deploy <- getEnvVarDefaultOption
    _OPT_DEPLOYMENT (`lookup` [("dev", DEV), ("test", TEST), ("prod", PROD)])
    "CI" (\str -> Just $ if str == "true" then TEST else DEV)
    DEV
  putStrLn $ ">>> Deployment environment is " ++ show deploy

  -- Determine color output preferences
  colors <- getEnvVarDefaultOption
    _OPT_COLOR (`lookup` [("true", True), ("false", False)])
    "NO_COLOR" (\_ -> Just False)
    True
  putStrLn $ ">>> Logging " ++ (if colors then "with" else "without") ++ " colors"

  if colors
    then return ()
    else SE.setEnv "TASTY_COLOR" "never"

  logHandle <- getWriteModeHandleOption _OPT_LOG_HANDLE stderr
  coutHandle <- getWriteModeHandleOption _OPT_CONSOLE_OUT stdout
  cinHandle <- getReadModeHandleOption _OPT_CONSOLE_IN stdin

  T.defaultMain
    . T.localOption (Deployment deploy)
    . T.localOption (RemoteEndRef $ Just pool)
    . T.localOption (LogHandle logHandle)
    . T.localOption (LogColors colors)
    . T.localOption (ConsoleOutHandle coutHandle)
    . T.localOption (ConsoleInHandle cinHandle)
    $ tree

  mapM_ hClose
    [ logHandle, coutHandle, cinHandle ]


getWriteModeHandleOption :: Text -> Handle -> IO Handle
getWriteModeHandleOption opt theDefault = do
  args <- fmap (fmap Text.pack) SE.getArgs
  case parseOptionWithArgument ("--" <> opt) args of
    Nothing -> do
      Text.putStrLn $ "Error: option '" <> opt <> "' is missing a required path argument"
      exitFailure
    Just Nothing -> return theDefault
    Just (Just path) -> case path of
      "stdout" -> return stdout
      "stderr" -> return stderr
      _ -> openFile (Text.unpack path) WriteMode


getReadModeHandleOption :: Text -> Handle -> IO Handle
getReadModeHandleOption opt theDefault = do
  args <- fmap (fmap Text.pack) SE.getArgs
  case parseOptionWithArgument ("--" <> opt) args of
    Nothing -> do
      Text.putStrLn $ "Error: option '" <> opt <> "' is missing a required path argument"
      exitFailure
    Just Nothing -> return theDefault
    Just (Just path) -> case path of
      "stdin" -> return stdin
      _ -> openFile (Text.unpack path) ReadMode


-- | Get the value of an option that can be controlled by either a command line flag or an environment variable, with the flag taking precedence.
getEnvVarDefaultOption
  :: Text -- ^ Flag name
  -> (Text -> Maybe a) -- ^ Mapping flag values to option values
  -> Text -- ^ Environment variable name
  -> (Text -> Maybe a) -- ^ Mapping environment variable values to option values
  -> a -- ^ Default option value (if neither flag nor env var is set)
  -> IO a
getEnvVarDefaultOption flag flagMap var varMap def = do
  args <- SE.getArgs
  case parseOptionWithArgument ("--" <> flag) (fmap Text.pack args) of

    -- Flag is present, but with no argument given.
    Nothing -> do
      Text.putStrLn $ "Error: option '" <> flag <> "' is missing a required argument"
      exitFailure

    -- Flag with argument is present.
    Just (Just value) ->
      case flagMap value of
        Just a -> return a
        Nothing -> do
          Text.putStrLn $ "Error: unrecognized value '" <> value <> "' for option '--" <> flag <> "'."
          exitFailure

    -- Flag not present; try to use the environment variable.
    Just Nothing -> do
      value <- SE.lookupEnv $ Text.unpack var
      case fmap Text.pack value of

        -- Environment variable is set.
        Just str ->
          case varMap str of
            Just a -> return a
            Nothing -> do
              Text.putStrLn $ "Error: unrecognized value '" <> str <>
                "' for environment variable '" <> var <> "'."
              exitFailure

        -- Environment variable not set; use default.
        Nothing -> return def



getRemoteEndRef :: IO (TVar RemoteEndPool)
getRemoteEndRef = do
  configPool <- fromMaybe mempty <$> getRemoteEndConfigPath
  optionPool <- fromMaybe mempty <$> getRemoteEndOptionString
  let pool = mappend configPool optionPool
  if pool == mempty
    then newTVarIO $ RemoteEndPool $ MS.fromList
      [ (Geckodriver, [RemoteEnd "localhost" 4444 ""])
      , (Chromedriver, [RemoteEnd "localhost" 9515 ""])
      ]
    else newTVarIO $ mappend configPool optionPool



getRemoteEndConfigPath :: IO (Maybe RemoteEndPool)
getRemoteEndConfigPath = do
  args <- fmap (fmap Text.pack) SE.getArgs
  case parseOptionWithArgument "--wd-remote-ends-config" args of
    Nothing -> do
      putStrLn "option --wd-remote-ends-config missing required path argument"
      exitFailure
    Just Nothing -> return Nothing
    Just (Just path) -> do
      str <- Text.readFile $ Text.unpack path
      case parseRemoteEndConfig str of
        Left err -> do
          Text.putStrLn err
          exitFailure
        Right x -> return (Just x)



getRemoteEndOptionString :: IO (Maybe RemoteEndPool)
getRemoteEndOptionString = do
  args <- fmap (fmap Text.pack) SE.getArgs
  case parseOptionWithArgument "--wd-remote-ends" args of
    Nothing -> do
      putStrLn "option --wd-remote-ends missing required argument"
      exitFailure
    Just Nothing -> return Nothing
    Just (Just str) ->
      case parseRemoteEndOption str of
        Left err -> do
          Text.putStrLn err
          exitFailure
        Right x -> return (Just x)



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

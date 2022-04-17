{- |
Module      : Web.Api.WebDriver.Monad
Description : A WebDriver session monad.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

A monad transformer for building WebDriver sessions.
-}

{-#
  LANGUAGE
    GADTs,
    Rank2Types,
    KindSignatures,
    RecordWildCards,
    OverloadedStrings
#-}

module Web.Api.WebDriver.Monad (
    WebDriverT
  , execWebDriverT
  , debugWebDriverT
  , checkWebDriverT

  , WebDriverTT()
  , execWebDriverTT
  , debugWebDriverTT
  , checkWebDriverTT
  , liftWebDriverTT

  , evalWDAct
  , Http.evalIO
  , evalWDActMockIO
  , Http.evalMockIO

  -- * Config
  , WebDriverConfig(..)
  , defaultWebDriverConfig
  , defaultWebDriverState
  , defaultWebDriverEnvironment
  , defaultWDEnv
  , defaultWebDriverLogOptions

  -- * API
  , fromState
  , modifyState
  , fromEnv
  , comment
  , wait
  , logDebug
  , logNotice
  , throwError
  , throwJsonError
  , throwHttpException
  , throwIOException
  , expect
  , expectIs
  , assert
  , catchError
  , catchJsonError
  , catchHttpException
  , catchIOException
  , catchAnyError
  , parseJson
  , lookupKeyJson
  , constructFromJson
  , httpGet
  , httpSilentGet
  , httpPost
  , httpSilentPost
  , httpDelete
  , httpSilentDelete
  , hPutStrLn
  , hPutStrLnBlocking
  , getStrLn
  , promptForString
  , promptForSecret
  , readFilePath
  , writeFilePath
  , fileExists
  , breakpointsOn
  , breakpointsOff
  , breakpoint
  , breakpointWith

  -- * Types
  , Http.E()
  , Http.JsonError(..)
  , WDError(..)
  , Http.R(..)
  , Http.LogOptions(..)
  , WDEnv(..)
  , ResponseFormat(..)
  , ApiVersion(..)
  , Outcome(..)
  , Http.Url
  , Http.HttpResponse(..)
  , WDLog(..)
  , Http.P(..)
  , WDAct(..)
  , Http.S(..)
  , WDState(..)
  , BreakpointSetting(..)

  -- * Logs
  , getAssertions
  , Http.logEntries
  , Http.printHttpLogs
  , Http.basicLogEntryPrinter
) where



import Prelude hiding (readFile, writeFile, putStrLn)

import Control.Concurrent.MVar
  ( MVar )
import Control.Exception
  ( IOException, try )
import Control.Lens
  ( (^.), (^?) )
import Control.Monad
  ( ap )
import Control.Monad.IO.Class
  ( MonadIO(..) )
import Control.Monad.Trans.Class
  ( MonadTrans(..) )
import Control.Monad.Trans.Identity
  ( IdentityT(..) )
import Data.Aeson
  ( Value(), Result(Success), toJSON, (.=), FromJSON, fromJSON, object )
import Data.Aeson.Encode.Pretty
  ( encodePretty )
import Data.Aeson.Lens
  ( key, _Value, _String )
import qualified Data.ByteString.Char8 as SC
  ( unpack )
import Data.ByteString.Lazy
  ( ByteString, readFile, writeFile )
import qualified Data.ByteString.Lazy.Char8 as LC
  ( unpack, pack )
import Data.List
  ( intercalate )
import Data.Text
  ( unpack, Text )
import qualified Network.HTTP.Client as N
  ( HttpException(..), HttpExceptionContent(..) )
import Network.Wreq
  ( Status, statusMessage, statusCode, responseStatus, defaults )
import System.Directory
  ( doesFileExist )
import System.IO
  ( Handle, hGetLine, hSetEcho, hGetEcho, stdout, stdin )
import System.IO.Error
  ( eofErrorType, doesNotExistErrorType, mkIOError )
import Test.QuickCheck
  ( Property )

import qualified Control.Monad.Script.Http as Http
import qualified Data.MockIO as Mock
import qualified Data.MockIO.FileSystem as FS

import Web.Api.WebDriver.Types
import Web.Api.WebDriver.Assert





-- | Wrapper type around `Http.HttpTT`; a stack of error, reader, writer, state, and prompt monad transformers.
newtype WebDriverTT
  (t :: (* -> *) -> * -> *)
  (eff :: * -> *)
  (a :: *)
  = WDT
    { unWDT :: Http.HttpTT WDError WDEnv WDLog WDState WDAct t eff a }

instance
  (Monad eff, Monad (t eff), MonadTrans t)
    => Functor (WebDriverTT t eff) where
  fmap f = WDT . fmap f . unWDT

instance
  (Monad eff, Monad (t eff), MonadTrans t)
    => Applicative (WebDriverTT t eff) where
  pure = return
  (<*>) = ap

instance
  (Monad eff, Monad (t eff), MonadTrans t)
    => Monad (WebDriverTT t eff) where
  return = WDT . return
  (WDT x) >>= f = WDT (x >>= (unWDT . f))

instance
  (MonadIO eff, MonadIO (t eff), MonadTrans t)
    => MonadIO (WebDriverTT t eff) where
  liftIO = WDT . Http.liftHttpTT . liftIO

-- | Lift a value from the inner transformed monad
liftWebDriverTT
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => t eff a -> WebDriverTT t eff a
liftWebDriverTT = WDT . Http.liftHttpTT

-- | Type representing configuration settings for a WebDriver session
data WebDriverConfig eff = WDConfig
  { _initialState :: Http.S WDState
  , _environment :: Http.R WDError WDLog WDEnv
  , _evaluator :: forall a. Http.P WDAct a -> eff a
  }

-- | Default `IO` effects
defaultWebDriverConfig :: WebDriverConfig IO
defaultWebDriverConfig = WDConfig
  { _initialState = defaultWebDriverState
  , _environment = defaultWebDriverEnvironment
  , _evaluator = Http.evalIO evalWDAct
  }

defaultWebDriverState :: Http.S WDState
defaultWebDriverState = Http.S
  { Http._httpOptions = defaults
  , Http._httpSession = Nothing
  , Http._userState = WDState
    { _sessionId = Nothing
    , _breakpoints = BreakpointsOff
    }
  }

defaultWebDriverEnvironment :: Http.R WDError WDLog WDEnv
defaultWebDriverEnvironment = Http.R
  { Http._logHandle = stdout
  , Http._logLock = Nothing
  , Http._logEntryPrinter = Http.basicLogEntryPrinter
  , Http._uid = ""
  , Http._logOptions = defaultWebDriverLogOptions
  , Http._httpErrorInject = promoteHttpResponseError
  , Http._env = defaultWDEnv
  }

-- | Uses default geckodriver settings
defaultWDEnv :: WDEnv
defaultWDEnv = WDEnv
  { _remoteHostname = "localhost"
  , _remotePort = 4444
  , _remotePath = ""
  , _dataPath = ""
  , _responseFormat = SpecFormat
  , _apiVersion = CR_2018_03_04
  , _stdin = stdin
  , _stdout = stdout
  }

-- | Noisy, JSON, in color, without headers.
defaultWebDriverLogOptions :: Http.LogOptions WDError WDLog
defaultWebDriverLogOptions = Http.trivialLogOptions
  { Http._logColor = True
  , Http._logJson = True
  , Http._logHeaders = False
  , Http._logSilent = False
  , Http._printUserError = printWDError
  , Http._printUserLog = printWDLog
  }



-- | Execute a `WebDriverTT` session.
execWebDriverTT
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverConfig eff
  -> WebDriverTT t eff a
  -> t eff (Either (Http.E WDError) a, Http.S WDState, Http.W WDError WDLog)
execWebDriverTT config = Http.execHttpTT
  (_initialState config) (_environment config) (_evaluator config) . unWDT

-- | Execute a `WebDriverTT` session, returning an assertion summary with the result.
debugWebDriverTT
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverConfig eff
  -> WebDriverTT t eff a
  -> t eff (Either String a, AssertionSummary)
debugWebDriverTT config session = do
  (result, _, w) <- execWebDriverTT config session
  let output = case result of
        Right a -> Right a
        Left e -> Left $ Http.printError (printWDError True) e
  return (output, summarize $ getAssertions $ Http.logEntries w)

-- | For testing with QuickCheck.
checkWebDriverTT
  :: (Monad eff, Monad (t eff), MonadTrans t, Show q)
  => WebDriverConfig eff
  -> (t eff (Either (Http.E WDError) a, Http.S WDState, Http.W WDError WDLog) -> IO q) -- ^ Condense to `IO`
  -> (q -> Bool) -- ^ Result check
  -> WebDriverTT t eff a
  -> Property
checkWebDriverTT config cond check =
  Http.checkHttpTT
    (_initialState config)
    (_environment config)
    (_evaluator config)
    cond check . unWDT





-- | `WebDriverTT` over `IdentityT`.
type WebDriverT eff a = WebDriverTT IdentityT eff a



-- | Execute a `WebDriverT` session.
execWebDriverT
  :: (Monad eff)
  => WebDriverConfig eff
  -> WebDriverT eff a
  -> eff (Either (Http.E WDError) a, Http.S WDState, Http.W WDError WDLog)
execWebDriverT config = runIdentityT . execWebDriverTT config

-- | Execute a `WebDriverT` session, returning an assertion summary with the result.
debugWebDriverT
  :: (Monad eff)
  => WebDriverConfig eff
  -> WebDriverT eff a
  -> eff (Either String a, AssertionSummary)
debugWebDriverT config session = do
  (result, _, w) <- execWebDriverT config session
  let output = case result of
        Right a -> Right a
        Left e -> Left $ Http.printError (printWDError True) e
  return (output, summarize $ getAssertions $ Http.logEntries w)

-- | For testing with QuickCheck
checkWebDriverT
  :: (Monad eff, Show q)
  => WebDriverConfig eff
  -> (eff (Either (Http.E WDError) t, Http.S WDState, Http.W WDError WDLog) -> IO q) -- ^ Condense to `IO`
  -> (q -> Bool) -- ^ Result check
  -> WebDriverT eff t
  -> Property
checkWebDriverT config cond = checkWebDriverTT config (cond . runIdentityT)



-- | Get a computed value from the state
fromState
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (Http.S WDState -> a) -> WebDriverTT t eff a
fromState = WDT . Http.gets

-- | Mutate the state
modifyState
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (Http.S WDState -> Http.S WDState) -> WebDriverTT t eff ()
modifyState = WDT . Http.modify

-- | Get a computed value from the environment
fromEnv
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (Http.R WDError WDLog WDEnv -> a) -> WebDriverTT t eff a
fromEnv = WDT . Http.reader

logDebug
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WDLog -> WebDriverTT t eff ()
logDebug = WDT . Http.logDebug

logNotice
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WDLog -> WebDriverTT t eff ()
logNotice = WDT . Http.logNotice

-- | Write a comment to the log.
comment
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => String -> WebDriverTT t eff ()
comment = WDT . Http.comment

-- | Suspend the current session. Handy when waiting for pages to load.
wait
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Int -- ^ Wait time in milliseconds
  -> WebDriverTT t eff ()
wait = WDT . Http.wait

throwError
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WDError -> WebDriverTT t eff a
throwError = WDT . Http.throwError

throwJsonError
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Http.JsonError -> WebDriverTT t eff a
throwJsonError = WDT . Http.throwJsonError

throwHttpException
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => N.HttpException -> WebDriverTT t eff a
throwHttpException = WDT . Http.throwHttpException

throwIOException
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => IOException -> WebDriverTT t eff a
throwIOException = WDT . Http.throwIOException

-- | Explicitly handle any of the error types thrown in `WebDriverTT`
catchAnyError
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff a
  -> (WDError -> WebDriverTT t eff a)
  -> (N.HttpException -> WebDriverTT t eff a)
  -> (IOException -> WebDriverTT t eff a)
  -> (Http.JsonError -> WebDriverTT t eff a)
  -> WebDriverTT t eff a
catchAnyError x hE hH hI hJ = WDT $ Http.catchAnyError (unWDT x)
  (unWDT . hE) (unWDT . hH) (unWDT . hI) (unWDT . hJ)

-- | Rethrows other error types
catchError
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff a
  -> (WDError -> WebDriverTT t eff a)
  -> WebDriverTT t eff a
catchError x h = WDT $ Http.catchError (unWDT x) (unWDT . h)

-- | Rethrows other error types
catchJsonError
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff a
  -> (Http.JsonError -> WebDriverTT t eff a)
  -> WebDriverTT t eff a
catchJsonError x h = WDT $ Http.catchJsonError (unWDT x) (unWDT . h)

-- | Rethrows other error types
catchHttpException
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff a
  -> (N.HttpException -> WebDriverTT t eff a)
  -> WebDriverTT t eff a
catchHttpException x h = WDT $ Http.catchHttpException (unWDT x) (unWDT . h)

-- | Rethrows other error types
catchIOException
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff a
  -> (IOException -> WebDriverTT t eff a)
  -> WebDriverTT t eff a
catchIOException x h = WDT $ Http.catchIOException (unWDT x) (unWDT . h)

-- | May throw a `JsonError`.
parseJson
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => ByteString -> WebDriverTT t eff Value
parseJson = WDT . Http.parseJson

-- | May throw a `JsonError`.
lookupKeyJson
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Text -> Value -> WebDriverTT t eff Value
lookupKeyJson k = WDT . Http.lookupKeyJson k

-- | May throw a `JsonError`.
constructFromJson
  :: (Monad eff, Monad (t eff), MonadTrans t, FromJSON a)
  => Value -> WebDriverTT t eff a
constructFromJson = WDT . Http.constructFromJson

-- | Capures `HttpException`s.
httpGet
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Http.Url -> WebDriverTT t eff Http.HttpResponse
httpGet = WDT . Http.httpGet

-- | Does not write request or response info to the log, except to note that a request occurred. Capures `HttpException`s.
httpSilentGet
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Http.Url -> WebDriverTT t eff Http.HttpResponse
httpSilentGet = WDT . Http.httpSilentGet

-- | Capures `HttpException`s.
httpPost
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Http.Url -> ByteString -> WebDriverTT t eff Http.HttpResponse
httpPost url = WDT . Http.httpPost url

-- | Does not write request or response info to the log, except to note that a request occurred. Capures `HttpException`s.
httpSilentPost
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Http.Url -> ByteString -> WebDriverTT t eff Http.HttpResponse
httpSilentPost url = WDT . Http.httpSilentPost url

-- | Capures `HttpException`s.
httpDelete
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Http.Url -> WebDriverTT t eff Http.HttpResponse
httpDelete = WDT . Http.httpDelete

-- | Does not write request or response info to the log, except to note that a request occurred. Capures `HttpException`s.
httpSilentDelete
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Http.Url -> WebDriverTT t eff Http.HttpResponse
httpSilentDelete = WDT . Http.httpSilentDelete

-- | Capures `IOException`s.
hPutStrLn
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Handle -> String -> WebDriverTT t eff ()
hPutStrLn h = WDT . Http.hPutStrLn h

-- | Capures `IOException`s.
hPutStrLnBlocking
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => MVar () -> Handle -> String -> WebDriverTT t eff ()
hPutStrLnBlocking lock h = WDT . Http.hPutStrLnBlocking lock h

promptWDAct
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WDAct a -> WebDriverTT t eff a
promptWDAct = WDT . Http.prompt . Http.P



instance
  (Monad eff, Monad (t eff), MonadTrans t)
    => Assert (WebDriverTT t eff) where
  assert = logNotice . LogAssertion





-- | Filter the assertions from a WebDriver log.
getAssertions :: [WDLog] -> [Assertion]
getAssertions xs = get xs
  where
    get [] = []
    get (w:ws) = case w of
      LogAssertion a -> a : get ws
      _ -> get ws



-- | Errors specific to WebDriver sessions.
data WDError
  = NoSession

  -- | See <https://w3c.github.io/webdriver/webdriver-spec.html#handling-errors>
  | ResponseError ResponseErrorCode String String (Maybe Value) Status

  | UnableToConnect
  | RemoteEndTimeout
  | UnhandledHttpException N.HttpException
  | ImageDecodeError String
  | UnexpectedValue String
  | UnexpectedResult Outcome String
  | BreakpointHaltError
  deriving Show

-- | Read-only environment variables specific to WebDriver.
data WDEnv = WDEnv
  { -- | Hostname of the remote WebDriver server
    _remoteHostname :: String

    -- | Port of the remote WebDriver server
  , _remotePort :: Int

    -- | Extra path for the remote WebDriver server
  , _remotePath :: String

    -- | Path where secret data is stored
  , _dataPath :: FilePath

    -- | Flag for the format of HTTP responses from the remote end. Needed because not all remote ends are spec-compliant.
  , _responseFormat :: ResponseFormat

    -- | Version of the WebDriver specification.
  , _apiVersion :: ApiVersion

  , _stdin :: Handle
  , _stdout :: Handle
  }

-- | Version of the WebDriver specification.
data ApiVersion
  = CR_2018_03_04 -- ^ Candidate Recommendation, March 4, 2018
  deriving (Eq, Show)

-- | Format flag for HTTP responses from the remote end. Chromedriver, for instance, is not spec-compliant. :)
data ResponseFormat
  = SpecFormat -- ^ Responses as described in the spec.
  | ChromeFormat -- ^ Responses as emitted by chromedriver.
  deriving (Eq, Show)

data BreakpointSetting
  = BreakpointsOn
  | BreakpointsOff
  deriving (Eq, Show)

-- | Includes a @Maybe String@ representing the current session ID, if one has been opened.
data WDState = WDState
  { _sessionId :: Maybe String
  , _breakpoints :: BreakpointSetting
  } deriving Show

breakpointsOn
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ()
breakpointsOn = modifyState $ \st -> st
  { Http._userState = (Http._userState st)
    { _breakpoints = BreakpointsOn
    }
  }

breakpointsOff
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ()
breakpointsOff = modifyState $ \st -> st
  { Http._userState = (Http._userState st)
    { _breakpoints = BreakpointsOff
    }
  }

-- | WebDriver specific log entries.
data WDLog
  = LogAssertion Assertion
  | LogSession SessionVerb
  | LogUnexpectedResult Outcome String
  | LogBreakpoint String
  deriving Show

-- | Pretty printer for log entries.
printWDLog :: Bool -> WDLog -> String
printWDLog _ w = show w

-- | Type representing an abstract outcome. Do with it what you will.
data Outcome = IsSuccess | IsFailure
  deriving (Eq, Show)

-- | Representation of the actions we can perform on a `Session` (in the @wreq@ sense).
data SessionVerb
  = Close | Open
  deriving (Eq, Show)

-- | WebDriver specific effects
data WDAct a where
  ReadFilePath :: FilePath -> WDAct (Either IOException ByteString)
  WriteFilePath :: FilePath -> ByteString -> WDAct (Either IOException ())
  FileExists :: FilePath -> WDAct (Either IOException Bool)

  HGetLine :: Handle -> WDAct (Either IOException String)
  HGetLineNoEcho :: Handle -> WDAct (Either IOException String)



-- | For validating responses. Throws an `UnexpectedValue` error if the two arguments are not equal according to their `Eq` instance.
expect
  :: (Monad eff, Monad (t eff), MonadTrans t, Eq a, Show a)
  => a
  -> a
  -> WebDriverTT t eff a
expect x y = if x == y
  then return y
  else throwError $ UnexpectedValue $
    "expected " ++ show x ++ " but got " ++ show y

-- | For validating responses. Throws an `UnexpectedValue` error if the `a` argument does not satisfy the predicate.
expectIs
  :: (Monad eff, Monad (t eff), MonadTrans t, Show a)
  => (a -> Bool)
  -> String -- ^ Human readable error label
  -> a
  -> WebDriverTT t eff a
expectIs p label x = if p x
  then return x
  else throwError $ UnexpectedValue $
    "expected " ++ label ++ " but got " ++ show x

-- | Promote semantic HTTP exceptions to typed errors.
promoteHttpResponseError :: N.HttpException -> Maybe WDError
promoteHttpResponseError e = case e of
  N.HttpExceptionRequest _ (N.StatusCodeException s r) -> do
    err <- r ^? key "value" . key "error" . _Value
    code <- case fromJSON err of
      Success m -> return m
      _ -> Nothing
    msg <- fmap unpack (r ^? key "value" . key "message" . _String)
    str <- fmap unpack (r ^? key "value" . key "stacktrace" . _String)
    let obj = r ^? key "value" . key "data" . _Value
    status <- s ^? responseStatus
    return $ ResponseError code msg str obj status

  N.HttpExceptionRequest _ (N.ConnectionFailure _) -> Just UnableToConnect

  N.HttpExceptionRequest _ N.ConnectionTimeout -> Just RemoteEndTimeout

  _ -> Just $ UnhandledHttpException e

-- | For pretty printing.
printWDError :: Bool -> WDError -> String
printWDError _ e = case e of
  NoSession -> "No session in progress"
  ResponseError _ msg trace obj status ->
    let
      code = status ^. statusCode
      smsg = status ^. statusMessage
    in
      (("Response: " ++ show code ++ " " ++ SC.unpack smsg ++ "\n") ++) $
      LC.unpack $ encodePretty $ object
        [ "error" .= toJSON code
        , "message" .= toJSON msg
        , "stacktrace" .= toJSON trace
        , "data" .= (toJSON <$> obj)
        ]
  UnableToConnect -> "Unable to connect to WebDriver server"
  RemoteEndTimeout -> "Remote End Timeout"
  UnhandledHttpException ex -> "Unhandled HTTP Exception: " ++ show ex
  ImageDecodeError msg -> "Image decode: " ++ msg
  UnexpectedValue msg -> "Unexpected value: " ++ msg
  UnexpectedResult r msg -> case r of
    IsSuccess -> "Unexpected success: " ++ msg
    IsFailure -> "Unexpected failure: " ++ msg
  BreakpointHaltError -> "Breakpoint Halt"

putStrLn
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => String -> WebDriverTT t eff ()
putStrLn str = do
  outH <- fromEnv (_stdout . Http._env)
  hPutStrLn outH str

getStrLn
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff String
getStrLn = do
  inH <- fromEnv (_stdin . Http._env)
  result <- promptWDAct $ HGetLine inH
  case result of
    Right string -> return string
    Left e -> throwIOException e

-- | Prompt for input on `stdin`.
promptForString
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => String -- ^ Prompt text
  -> WebDriverTT t eff String
promptForString prompt =
  putStrLn prompt >> getStrLn

-- | Prompt for input on `stdin`, but do not echo the typed characters back to the terminal -- handy for getting suuper secret info.
promptForSecret
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => String -- ^ Prompt text
  -> WebDriverTT t eff String
promptForSecret prompt = do
  outH <- fromEnv (_stdout . Http._env)
  inH <- fromEnv (_stdin . Http._env)
  hPutStrLn outH prompt
  result <- promptWDAct $ HGetLineNoEcho inH
  case result of
    Right string -> return string
    Left e -> throwIOException e

-- | Captures `IOException`s
readFilePath
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => FilePath
  -> WebDriverTT t eff ByteString
readFilePath path = do
  result <- promptWDAct $ ReadFilePath path
  case result of
    Right bytes -> return bytes
    Left e -> throwIOException e

-- | Captures `IOException`s
writeFilePath
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => FilePath
  -> ByteString
  -> WebDriverTT t eff ()
writeFilePath path bytes = do
  result <- promptWDAct $ WriteFilePath path bytes
  case result of
    Right () -> return ()
    Left e -> throwIOException e

-- | Captures `IOException`s
fileExists
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => FilePath
  -> WebDriverTT t eff Bool
fileExists path = do
  result <- promptWDAct $ FileExists path
  case result of
    Right p -> return p
    Left e -> throwIOException e



data BreakpointAction
  = BreakpointContinue
  | BreakpointHalt
  | BreakpointDump -- ^ Show the current state and environment
  | BreakpointSilence -- ^ Turn breakpoints off and continue
  | BreakpointAct -- ^ Client-supplied action
  deriving (Eq, Show)

parseBreakpointAction :: String -> Maybe BreakpointAction
parseBreakpointAction str = case str of
  "c" -> Just BreakpointContinue
  "h" -> Just BreakpointHalt
  "d" -> Just BreakpointDump
  "s" -> Just BreakpointSilence
  "a" -> Just BreakpointAct
  _ -> Nothing

breakpointMessage
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => String -> Maybe String -> WebDriverTT t eff ()
breakpointMessage msg custom = do
  putStrLn "=== BREAKPOINT ==="
  putStrLn msg
  putStrLn "c : continue"
  putStrLn "h : halt"
  putStrLn "d : dump webdriver state"
  putStrLn "s : turn breakpoints off and continue"
  case custom of
    Just act -> putStrLn $ "a : " ++ act
    Nothing -> return ()
  putStrLn "=================="

breakpointWith
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => String
  -> Maybe (String, WebDriverTT t eff ())
  -> WebDriverTT t eff ()
breakpointWith msg act = do
  bp <- fromState (_breakpoints . Http._userState)
  case bp of
    BreakpointsOff -> return ()
    BreakpointsOn -> do
      logNotice $ LogBreakpoint msg
      let
        (actionDescription, action) = case act of
          Nothing -> (Nothing, return ())
          Just (title, action') -> (Just title, action')
      breakpointMessage msg actionDescription
      command <- getStrLn
      case parseBreakpointAction command of
        Just BreakpointContinue -> return ()
        Just BreakpointHalt -> throwError BreakpointHaltError
        Just BreakpointDump -> do
          putStrLn "=== DUMP ========="
          fromState dumpState >>= putStrLn
          fromEnv dumpEnv >>= putStrLn
          putStrLn "=================="
          breakpointWith msg act
        Just BreakpointSilence -> breakpointsOff
        Just BreakpointAct -> action
        Nothing -> do
          putStrLn $ "Unrecognized breakpoint option '" ++ command ++ "'"
          breakpointWith msg act

breakpoint
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => String
  -> WebDriverTT t eff ()
breakpoint msg = breakpointWith msg Nothing

dumpState :: Http.S WDState -> String
dumpState Http.S{..} = intercalate "\n"
  [ "Session ID: " ++ show (_sessionId _userState)
  , show (_breakpoints _userState)
  ]

dumpEnv :: Http.R WDError WDLog WDEnv -> String
dumpEnv Http.R{..} = intercalate "\n"
  [ "Remote Host: " ++ (_remoteHostname _env)
  , "Remote Port: " ++ show (_remotePort _env)
  , "Remote Path: " ++ (_remotePath _env)
  , "Data Path: " ++ (_dataPath _env)
  , "Response Format: " ++ show (_responseFormat _env)
  , "API Version: " ++ show (_apiVersion _env)
  ]



-- | Standard `IO` evaluator for `WDAct`.
evalWDAct :: WDAct a -> IO a
evalWDAct act = case act of
  ReadFilePath path -> try $ readFile path
  WriteFilePath path bytes -> try $ writeFile path bytes
  FileExists path -> try $ doesFileExist path

  HGetLine handle -> try $ do
    hGetLine handle

  HGetLineNoEcho handle -> try $ do
    echo <- hGetEcho handle
    hSetEcho handle False
    secret <- hGetLine handle
    hSetEcho handle echo
    return secret



-- | Standard `Mock.MockIO` evaluator for `WDAct`.
evalWDActMockIO :: WDAct a -> Mock.MockIO u a
evalWDActMockIO act = case act of
  ReadFilePath path -> do
    Mock.incrementTimer 1
    world <- Mock.getMockWorld
    let result = FS.getLines (Left path) $ Mock._files world
    case result of
      Nothing -> do
        return $ Left $ mkIOError doesNotExistErrorType "" Nothing (Just path)
      Just lns -> return $ Right $ LC.pack $ unlines lns

  WriteFilePath path bytes -> do
    Mock.incrementTimer 1
    fmap Right $ Mock.modifyMockWorld $ \w -> w
      { Mock._files = FS.writeLines (Left path) [LC.unpack bytes] $ Mock._files w }

  FileExists path -> do
    Mock.incrementTimer 1
    world <- Mock.getMockWorld
    return $ Right $ FS.fileExists (Left path) $ Mock._files world

  HGetLine handle -> do
    Mock.incrementTimer 1
    world <- Mock.getMockWorld
    let dne = mkIOError doesNotExistErrorType "" (Just handle) Nothing
    let eof = mkIOError eofErrorType "" (Just handle) Nothing
    let result = FS.readLine dne eof (Right handle) $ Mock._files world
    case result of
      Left err -> return $ Left err
      Right (str, fs) -> do
        Mock.modifyMockWorld $ \w -> w { Mock._files = fs }
        return $ Right str

  HGetLineNoEcho handle -> do
    Mock.incrementTimer 1
    world <- Mock.getMockWorld
    let dne = mkIOError doesNotExistErrorType "" (Just handle) Nothing
    let eof = mkIOError eofErrorType "" (Just handle) Nothing
    let result = FS.readLine dne eof (Right handle) $ Mock._files world
    case result of
      Left err -> return $ Left err
      Right (str, fs) -> do
        Mock.modifyMockWorld $ \w -> w { Mock._files = fs }
        return $ Right str

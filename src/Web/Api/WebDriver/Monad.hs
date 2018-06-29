{- |
Module      : Web.Api.WebDriver.Monad
Description : A WebDriver session monad.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

A monad and monad transformer for building WebDriver sessions.
-}

{-# LANGUAGE GADTs, Rank2Types, OverloadedStrings #-}
module Web.Api.WebDriver.Monad (
    WebDriver
  , execWebDriver
  , debugWebDriver
  , checkWebDriver

  , WebDriverT()
  , execWebDriverT
  , debugWebDriverT
  , checkWebDriverT
  , liftWebDriverT
  , IdentityT(..)

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
  , throwError
  , throwJsonError
  , throwHttpException
  , throwIOException
  , expect
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
  , promptForString
  , promptForSecret
  , readFilePath
  , writeFilePath
  , fileExists

  -- * Types
  , Http.E(..)
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

  -- * Logs
  , getAssertions
  , Http.logEntries
) where



import Prelude hiding (readFile, writeFile)

import Control.Concurrent.MVar
  ( MVar )
import Control.Exception
  ( IOException, try )
import Control.Lens
  ( (^.), (^?) )
import Control.Monad
  ( ap )
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
import Data.Functor.Identity
  ( Identity(..) )
import Data.IORef
  ( IORef, newIORef, readIORef, writeIORef )
import qualified Data.Map.Strict as M
  ( Map, fromList )
import Data.Text
  ( pack, unpack, Text )
import qualified Network.HTTP.Client as N
  ( HttpException(..), HttpExceptionContent(..) )
import Network.Wreq
  ( Status, statusMessage, statusCode, responseStatus, defaults )
import System.Directory
  ( doesFileExist )
import System.IO
  ( Handle, hGetLine, hSetEcho, hGetEcho, hFlush, stdout, stdin )
import System.IO.Error
  ( eofErrorType, doesNotExistErrorType, mkIOError )
import Test.QuickCheck
  ( Property )

import qualified Control.Monad.Script.Http as Http
import qualified Data.MockIO as Mock
import qualified Data.MockIO.FileSystem as FS

import Web.Api.WebDriver.Types
import Web.Api.WebDriver.Assert





-- | Wrapper type around `Http.HttpT`; a stack of error, reader, writer, state, and prompt monads.
newtype WebDriverT m a = WDT
  { unWDT :: Http.HttpT WDError WDEnv WDLog WDState WDAct m a }

instance Functor (WebDriverT m) where
  fmap f = WDT . fmap f . unWDT

instance Applicative (WebDriverT m) where
  pure = return
  (<*>) = ap

instance Monad (WebDriverT m) where
  return = WDT . return
  (WDT x) >>= f = WDT (x >>= (unWDT . f))

-- | Lift a value from the inner monad
liftWebDriverT :: (Monad m) => m a -> WebDriverT m a
liftWebDriverT = WDT . Http.liftHttpT

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
    }
  }

defaultWebDriverEnvironment :: Http.R WDError WDLog WDEnv
defaultWebDriverEnvironment = Http.R
  { Http._logHandle = stdout
  , Http._logLock = Nothing
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



-- | Execute a `WebDriverT` session.
execWebDriverT
  :: (Monad eff, Monad (m eff))
  => WebDriverConfig eff
  -> (forall u. eff u -> m eff u) -- ^ Lift effects to the inner monad
  -> WebDriverT (m eff) a
  -> m eff (Either (Http.E WDError) a, Http.S WDState, Http.W WDError WDLog)
execWebDriverT config lift = Http.execHttpTM
  (_initialState config) (_environment config) (_evaluator config) lift . unWDT

-- | Execute a `WebDriverT` session, returning an assertion summary with the result.
debugWebDriverT
  :: (Monad eff, Monad (m eff))
  => WebDriverConfig eff
  -> (forall u. eff u -> m eff u) -- ^ Lift effects to the inner monad
  -> WebDriverT (m eff) a
  -> m eff (Either String a, AssertionSummary)
debugWebDriverT config lift session = do
  (result, _, w) <- execWebDriverT config lift session
  let output = case result of
        Right a -> Right a
        Left e -> Left $ Http.printError (printWDError True) e
  return (output, summarize $ getAssertions $ Http.logEntries w)

-- | For testing with QuickCheck.
checkWebDriverT
  :: (Monad eff, Monad (m eff))
  => WebDriverConfig eff
  -> (forall u. eff u -> m eff u) -- ^ Lift effects to the inner monad
  -> (m eff (Either (Http.E WDError) t, Http.S WDState, Http.W WDError WDLog) -> IO q) -- ^ Condense to `IO`
  -> (q -> Bool) -- ^ Result check
  -> WebDriverT (m eff) t
  -> Property
checkWebDriverT config lift cond check =
  Http.checkHttpTM
    (_initialState config)
    (_environment config)
    (_evaluator config)
    lift cond check . unWDT





-- | `WebDriverT` over `IdentityT`.
type WebDriver eff a = WebDriverT (IdentityT eff) a

-- | The identity monad transformer.
newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance (Monad m) => Monad (IdentityT m) where
  return = IdentityT . return
  x >>= f = IdentityT $ runIdentityT x >>= (runIdentityT . f)

instance (Functor m) => Functor (IdentityT m) where
  fmap f = IdentityT . fmap f . runIdentityT

instance (Monad m) => Applicative (IdentityT m) where
  pure = return
  (<*>) = ap



-- | Execute a `WebDriver` session.
execWebDriver
  :: (Monad eff)
  => WebDriverConfig eff
  -> WebDriver eff a
  -> eff (Either (Http.E WDError) a, Http.S WDState, Http.W WDError WDLog)
execWebDriver config = runIdentityT . execWebDriverT config IdentityT

-- | Execute a `WebDriver` session, returning an assertion summary with the result.
debugWebDriver
  :: (Monad eff)
  => WebDriverConfig eff
  -> WebDriver eff a
  -> eff (Either String a, AssertionSummary)
debugWebDriver config session = do
  (result, _, w) <- execWebDriver config session
  let output = case result of
        Right a -> Right a
        Left e -> Left $ Http.printError (printWDError True) e
  return (output, summarize $ getAssertions $ Http.logEntries w)

-- | For testing with QuickCheck
checkWebDriver
  :: (Monad eff)
  => WebDriverConfig eff
  -> (eff (Either (Http.E WDError) t, Http.S WDState, Http.W WDError WDLog) -> IO q) -- ^ Condense to `IO`
  -> (q -> Bool) -- ^ Result check
  -> WebDriver eff t
  -> Property
checkWebDriver config cond = checkWebDriverT config IdentityT (cond . runIdentityT)



-- | Get a computed value from the state
fromState :: (Http.S WDState -> a) -> WebDriverT m a
fromState = WDT . Http.gets

-- | Mutate the state
modifyState :: (Http.S WDState -> Http.S WDState) -> WebDriverT m ()
modifyState = WDT . Http.modify

-- | Get a computed value from the environment
fromEnv :: (Http.R WDError WDLog WDEnv -> a) -> WebDriverT m a
fromEnv = WDT . Http.reader

logEntry :: WDLog -> WebDriverT m ()
logEntry = WDT . Http.logEntry

-- | Write a comment to the log.
comment :: String -> WebDriverT m ()
comment = WDT . Http.comment

-- | In milliseconds
wait :: Int -> WebDriverT m ()
wait = WDT . Http.wait

throwError :: WDError -> WebDriverT m a
throwError = WDT . Http.throwError

throwJsonError :: Http.JsonError -> WebDriverT m a
throwJsonError = WDT . Http.throwJsonError

throwHttpException :: N.HttpException -> WebDriverT m a
throwHttpException = WDT . Http.throwHttpException

throwIOException :: IOException -> WebDriverT m a
throwIOException = WDT . Http.throwIOException

-- | Explicitly handle any of the error types thrown in `WebDriverT`
catchAnyError
  :: WebDriverT m a
  -> (WDError -> WebDriverT m a)
  -> (N.HttpException -> WebDriverT m a)
  -> (IOException -> WebDriverT m a)
  -> (Http.JsonError -> WebDriverT m a)
  -> WebDriverT m a
catchAnyError x hE hH hI hJ = WDT $ Http.catchAnyError (unWDT x)
  (unWDT . hE) (unWDT . hH) (unWDT . hI) (unWDT . hJ)

-- | Rethrows other error types
catchError :: WebDriverT m a -> (WDError -> WebDriverT m a) -> WebDriverT m a
catchError x h = WDT $ Http.catchError (unWDT x) (unWDT . h)

-- | Rethrows other error types
catchJsonError :: WebDriverT m a -> (Http.JsonError -> WebDriverT m a) -> WebDriverT m a
catchJsonError x h = WDT $ Http.catchJsonError (unWDT x) (unWDT . h)

-- | Rethrows other error types
catchHttpException :: WebDriverT m a -> (N.HttpException -> WebDriverT m a) -> WebDriverT m a
catchHttpException x h = WDT $ Http.catchHttpException (unWDT x) (unWDT . h)

-- | Rethrows other error types
catchIOException :: WebDriverT m a -> (IOException -> WebDriverT m a) -> WebDriverT m a
catchIOException x h = WDT $ Http.catchIOException (unWDT x) (unWDT . h)

-- | May throw a `JsonError`.
parseJson :: ByteString -> WebDriverT m Value
parseJson = WDT . Http.parseJson

-- | May throw a `JsonError`.
lookupKeyJson :: Text -> Value -> WebDriverT m Value
lookupKeyJson key = WDT . Http.lookupKeyJson key

-- | May throw a `JsonError`.
constructFromJson :: (FromJSON a) => Value -> WebDriverT m a
constructFromJson = WDT . Http.constructFromJson

-- | Capures `HttpException`s.
httpGet :: Http.Url -> WebDriverT m Http.HttpResponse
httpGet = WDT . Http.httpGet

-- | Does not write request or response info to the log, except to note that a request occurred. Capures `HttpException`s.
httpSilentGet :: Http.Url -> WebDriverT m Http.HttpResponse
httpSilentGet = WDT . Http.httpSilentGet

-- | Capures `HttpException`s.
httpPost :: Http.Url -> ByteString -> WebDriverT m Http.HttpResponse
httpPost url = WDT . Http.httpPost url

-- | Does not write request or response info to the log, except to note that a request occurred. Capures `HttpException`s.
httpSilentPost :: Http.Url -> ByteString -> WebDriverT m Http.HttpResponse
httpSilentPost url = WDT . Http.httpSilentPost url

-- | Capures `HttpException`s.
httpDelete :: Http.Url -> WebDriverT m Http.HttpResponse
httpDelete = WDT . Http.httpDelete

-- | Does not write request or response info to the log, except to note that a request occurred. Capures `HttpException`s.
httpSilentDelete :: Http.Url -> WebDriverT m Http.HttpResponse
httpSilentDelete = WDT . Http.httpSilentDelete

-- | Capures `IOException`s.
hPutStrLn :: Handle -> String -> WebDriverT m ()
hPutStrLn h = WDT . Http.hPutStrLn h

-- | Capures `IOException`s.
hPutStrLnBlocking :: MVar () -> Handle -> String -> WebDriverT m ()
hPutStrLnBlocking lock h = WDT . Http.hPutStrLnBlocking lock h

promptWDAct :: WDAct a -> WebDriverT m a
promptWDAct = WDT . Http.prompt . Http.P



instance Assert (WebDriverT m) where
  assert = logEntry . LogAssertion





-- | Filter the assertions from a `WebDriver` log.
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

-- | Includes a @Maybe String@ representing the current session ID, if one has been opened.
newtype WDState = WDState
  { _sessionId :: Maybe String
  } deriving Show

-- | WebDriver specific log entries.
data WDLog
  = LogAssertion Assertion
  | LogSession SessionVerb
  | LogUnexpectedResult Outcome String
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
  :: (Monad m, Eq a, Show a)
  => a
  -> a
  -> WebDriverT m a
expect x y = if x == y
  then return y
  else throwError $ UnexpectedValue $
    "expected " ++ show x ++ " but got " ++ show y

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
printWDError json e = case e of
  NoSession -> "No session in progress"
  ResponseError code msg trace obj status ->
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
  UnhandledHttpException e -> "Unhandled HTTP Exception: " ++ show e
  ImageDecodeError msg -> "Image decode: " ++ msg
  UnexpectedValue msg -> "Unexpected value: " ++ msg
  UnexpectedResult r msg -> case r of
    IsSuccess -> "Unexpected success: " ++ msg
    IsFailure -> "Unexpected failure: " ++ msg

-- | Prompt for input on `stdin`.
promptForString
  :: (Monad m)
  => String -- ^ Prompt text
  -> WebDriverT m String
promptForString prompt = do
  outH <- fromEnv (_stdout . Http._env)
  inH <- fromEnv (_stdin . Http._env)
  hPutStrLn outH prompt
  result <- promptWDAct $ HGetLine inH
  case result of
    Right string -> return string
    Left e -> throwIOException e

-- | Prompt for input on `stdin`, but do not echo the typed characters back to the terminal -- handy for getting suuper secret info.
promptForSecret
  :: (Monad m)
  => String -- ^ Prompt text
  -> WebDriverT m String
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
  :: (Monad m)
  => FilePath
  -> WebDriverT m ByteString
readFilePath path = do
  result <- promptWDAct $ ReadFilePath path
  case result of
    Right bytes -> return bytes
    Left e -> throwIOException e

-- | Captures `IOException`s
writeFilePath
  :: (Monad m)
  => FilePath
  -> ByteString
  -> WebDriverT m ()
writeFilePath path bytes = do
  result <- promptWDAct $ WriteFilePath path bytes
  case result of
    Right () -> return ()
    Left e -> throwIOException e

-- | Captures `IOException`s
fileExists
  :: (Monad m)
  => FilePath
  -> WebDriverT m Bool
fileExists path = do
  result <- promptWDAct $ FileExists path
  case result of
    Right p -> return p
    Left e -> throwIOException e



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

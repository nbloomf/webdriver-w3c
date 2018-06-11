{- |
Module      : Web.Api.WebDriver.Monad
Description : A WebDriver session monad.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

The `WebDriver` type is a specialization of `Http` with additional context relevant to the WebDriver spec.
-}

{-# LANGUAGE GADTs, Rank2Types, OverloadedStrings #-}
module Web.Api.WebDriver.Monad (
    WebDriver
  , execWebDriver
  , debugWebDriver
  , WebDriverConfig(..)
  , defaultWDConfig

  -- * Errors
  , Http.E(..)
  , Http.printE
  , Http.JsonError(..)
  , WDError(..)
  , throwError
  , catch
  , throwJsonError
  , expect
  , printWDError
  , promoteHttpResponseError

  -- * Environment
  , Http.R(..)
  , Http.LogOptions(..)
  , WDEnv(..)
  , ResponseFormat(..)
  , ApiVersion(..)
  , fromEnv
  , Outcome(..)
  , Http.Url
  , Http.HttpResponse(..)

  -- * Logs
  , WDLog(..)
  , printWDLog
  , log
  , comment
  , wait
  , getAssertions
  , Http.logEntries

  -- * State
  , Http.S(..)
  , WDState(..)
  , fromState
  , modifyState

  -- * Effects
  , Http.P(..)
  , WDAct(..)
  , evalWDAct
  , evalWDActMockIO
  , Http.evalIO

  -- * API
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

  , readFilePath
  , writeFilePath
  , fileExists

  , setSessionId

  -- * Helpers
  , theRemoteUrl
  , theRemoteUrlWithSession

  -- * Shell
  , wdInit
  , wdHttp

  -- * Testing
  , checkWebDriverM
  , checkWebDriverMockIO
) where

import Prelude hiding (log, readFile, writeFile)

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
import Data.IORef
  ( IORef, newIORef, readIORef, writeIORef )
import qualified Data.Map.Strict as M
  ( Map )
import Data.Text
  ( pack, unpack, Text )
import qualified Network.HTTP.Client as N
  ( HttpException(..), HttpExceptionContent(..) )
import Network.Wreq
  ( Status, statusMessage, statusCode, responseStatus )
import System.Directory
  ( doesFileExist )
import System.IO
  ( Handle, hGetLine, hSetEcho, hGetEcho, hFlush )

import qualified Control.Monad.Script.Http as Http
import qualified Control.Monad.Script.Http.MockIO as Mock

import Web.Api.WebDriver.Types
import Web.Api.WebDriver.Assert



-- | Wrapper type around `Http.Http`; a stack of error, reader, writer, state, and prompt monads.
newtype WebDriver a = WD
  { unWD :: Http.Http WDError WDEnv WDLog WDState WDAct a }

-- | Execute a `WebDriver` session.
execWebDriver
 :: (Monad m)
 => WebDriverConfig m
 -> WebDriver a
 -> m (Either (Http.E WDError) a, Http.S WDState, Http.W WDError WDLog)
execWebDriver config = Http.execHttpM
  (_initialState config) (_environment config) (_evaluator config) . unWD

-- | Execute a `WebDriver` session, returning an assertion summary with the result.
debugWebDriver
  :: (Monad m)
  => WebDriverConfig m
  -> WebDriver a
  -> m (Either (Http.E WDError) a, AssertionSummary)
debugWebDriver config session = do
  (result, _, w) <- execWebDriver config session
  return (result, summarize $ getAssertions $ Http.logEntries w)

-- | Execute a `WebDriver` session, mapping the result to `IO`. Used for testing.
checkWebDriverM
  :: (Monad m)
  => WebDriverConfig m
  -> (forall a. m a -> IO a)
  -> ((Either (Http.E WDError) t, Http.S WDState, Http.W WDError WDLog) -> q)
  -> WebDriver t
  -> IO q
checkWebDriverM config toIO check =
  Mock.checkHttpM
    (_initialState config)
    (_environment config)
    (_evaluator config)
    toIO check . unWD

-- | Execute a `WebDriver` session in `Mock.MockIO`. Used for testing.
checkWebDriverMockIO
  :: WebDriverConfig (Mock.MockIO u)
  -> Mock.MockWorld u
  -> (((Either (Http.E WDError) t, Http.S WDState, Http.W WDError WDLog), Mock.MockWorld u) -> q)
  -> WebDriver t
  -> q
checkWebDriverMockIO config world check =
  Mock.checkHttpMockIO
    (_initialState config)
    (_environment config)
    world
    (_evaluator config)
    check . unWD

data WebDriverConfig m = WDConfig
  { _initialState :: Http.S WDState
  , _environment :: Http.R WDError WDLog WDEnv
  , _evaluator :: (forall a. Http.P WDAct a -> m a)
  }

defaultWDConfig :: WebDriverConfig IO
defaultWDConfig = WDConfig
  { _initialState = undefined
  , _environment = undefined
  , _evaluator = undefined
  }

instance Functor WebDriver where
  fmap f = WD . fmap f . unWD

instance Applicative WebDriver where
  pure = return
  (<*>) = ap

instance Monad WebDriver where
  return = WD . return
  (WD x) >>= f = WD (x >>= (unWD . f))

fromState :: (Http.S WDState -> a) -> WebDriver a
fromState f = WD $ Http.gets f

fromEnv :: (Http.R WDError WDLog WDEnv -> a) -> WebDriver a
fromEnv f = WD $ Http.reader f

modifyState :: (Http.S WDState -> Http.S WDState) -> WebDriver ()
modifyState f = WD $ Http.modify f

log :: WDLog -> WebDriver ()
log = WD . Http.log

-- | Write a comment to the log.
comment :: String -> WebDriver ()
comment = WD . Http.comment

wait :: Int -> WebDriver ()
wait = WD . Http.wait

throwError :: Http.E WDError -> WebDriver a
throwError = WD . Http.throwError

catch :: WebDriver a -> (Http.E WDError -> WebDriver a) -> WebDriver a
catch x h = WD $ Http.catch (unWD x) (unWD . h)

throwJsonError :: Http.JsonError -> WebDriver a
throwJsonError = WD . Http.throwJsonError

parseJson :: ByteString -> WebDriver Value
parseJson = WD . Http.parseJson

lookupKeyJson :: Text -> Value -> WebDriver Value
lookupKeyJson key = WD . Http.lookupKeyJson key

constructFromJson :: (FromJSON a) => Value -> WebDriver a
constructFromJson = WD . Http.constructFromJson

-- | Filter the assertions from a `WebDriver` log.
getAssertions :: [WDLog] -> [Assertion]
getAssertions xs = get xs
  where
    get [] = []
    get (w:ws) = case w of
      LogAssertion a -> a : get ws
      _ -> get ws

-- | Capures `HttpException`s.
httpGet :: Http.Url -> WebDriver Http.HttpResponse
httpGet = WD . Http.httpGet

-- | Does not write request or response info to the log, except to note that a request occurred. Capures `HttpException`s.
httpSilentGet :: Http.Url -> WebDriver Http.HttpResponse
httpSilentGet = WD . Http.httpSilentGet

-- | Capures `HttpException`s.
httpPost :: Http.Url -> ByteString -> WebDriver Http.HttpResponse
httpPost url = WD . Http.httpPost url

-- | Does not write request or response info to the log, except to note that a request occurred. Capures `HttpException`s.
httpSilentPost :: Http.Url -> ByteString -> WebDriver Http.HttpResponse
httpSilentPost url = WD . Http.httpSilentPost url

-- | Capures `HttpException`s.
httpDelete :: Http.Url -> WebDriver Http.HttpResponse
httpDelete = WD . Http.httpDelete

-- | Does not write request or response info to the log, except to note that a request occurred. Capures `HttpException`s.
httpSilentDelete :: Http.Url -> WebDriver Http.HttpResponse
httpSilentDelete = WD . Http.httpSilentDelete

-- | Capures `HttpException`s.
hPutStrLn :: Handle -> String -> WebDriver ()
hPutStrLn h = WD. Http.hPutStrLn h

-- | Capures `HttpException`s.
hPutStrLnBlocking :: MVar () -> Handle -> String -> WebDriver ()
hPutStrLnBlocking lock h = WD. Http.hPutStrLnBlocking lock h



-- | Errors specific to WebDriver sessions.
data WDError
  = NoSession
  | ResponseError ResponseErrorCode String String (Maybe Value) Status -- ^ See <https://w3c.github.io/webdriver/webdriver-spec.html#handling-errors>
  | UnableToConnect
  | RemoteEndTimeout
  | UnhandledHttpException N.HttpException
  | ImageDecodeError String
  | UnexpectedValue String
  | UnexpectedResult Outcome String
  deriving Show

-- | Read-only environment variables specific to WebDriver.
data WDEnv = WDEnv
  { _remoteHostname :: String -- ^ Hostname of the remote WebDriver server
  , _remotePort :: Int -- ^ Port of the remote WebDriver server
  , _remotePath :: String -- ^ Extra path for the remote WebDriver server
  , _secretsPath :: FilePath -- ^ Path where secret data is stored
  , _vars :: M.Map String String -- ^ Named constants; this makes it possible to e.g. run the same WebDriver session against both a test and production environment on different hosts.
  , _responseFormat :: ResponseFormat -- ^ Flag for the format of HTTP responses from the remote end. E.g., chromedriver reponses are not spec-compliant.
  , _apiVersion :: ApiVersion -- ^ Version of the WebDriver specification.
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



-- | Url of the remote WebDriver server.
theRemoteUrl :: WebDriver String
theRemoteUrl = do
  host <- fromEnv (_remoteHostname . Http._userEnv)
  port <- fromEnv (_remotePort . Http._userEnv)
  path <- fromEnv (_remotePath . Http._userEnv)
  return $ concat [ "http://", host, ":", show port, path]

-- | Url of the remote WebDriver server, with session ID.
theRemoteUrlWithSession
  :: WebDriver String
theRemoteUrlWithSession = do
  st <- fromState (_sessionId . Http._userState)
  case st of
    Nothing -> throwError $ Http.E NoSession
    Just session_id -> do
      baseUrl <- theRemoteUrl
      return $ concat [ baseUrl, "/session/", session_id ]

-- | For validating responses. Throws an `UnexpectedValue` error if the two arguments are not equal according to their `Eq` instance.
expect :: (Eq a, Show a) => a -> a -> WebDriver a
expect x y = if x == y
  then return y
  else throwError $ Http.E $ UnexpectedValue $
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
  :: String -- ^ Prompt text
  -> WebDriver String
promptForString prompt = do
  outH <- fromEnv (_stdout . Http._userEnv)
  inH <- fromEnv (_stdin . Http._userEnv)
  hPutStrLn outH prompt
  result <- WD $ Http.prompt $ Http.P $ HGetLine inH
  case result of
    Right string -> return string
    Left e -> throwError $ Http.E_IO e

-- | Prompt for input on `stdin`, but do not echo the typed characters back to the terminal -- handy for getting suuper secret info.
promptForSecret
  :: String -- ^ Prompt text
  -> WebDriver String
promptForSecret prompt = do
  outH <- fromEnv (_stdout . Http._userEnv)
  inH <- fromEnv (_stdin . Http._userEnv)
  hPutStrLn outH prompt
  result <- WD $ Http.prompt $ Http.P $ HGetLineNoEcho inH
  case result of
    Right string -> return string
    Left e -> throwError $ Http.E_IO e

-- | Captures `IOException`s
readFilePath :: FilePath -> WebDriver ByteString
readFilePath path = do
  result <- WD $ Http.prompt $ Http.P $ ReadFilePath path
  case result of
    Right bytes -> return bytes
    Left e -> throwError $ Http.E_IO e

-- | Captures `IOException`s
writeFilePath :: FilePath -> ByteString -> WebDriver ()
writeFilePath path bytes = do
  result <- WD $ Http.prompt $ Http.P $ WriteFilePath path bytes
  case result of
    Right () -> return ()
    Left e -> throwError $ Http.E_IO e

-- | Captures `IOException`s
fileExists :: FilePath -> WebDriver Bool
fileExists path = do
  result <- WD $ Http.prompt $ Http.P $ FileExists path
  case result of
    Right p -> return p
    Left e -> throwError $ Http.E_IO e

-- | Set the session id of a `WDState`.
setSessionId :: Maybe String -> Http.S WDState -> Http.S WDState
setSessionId x st = st { Http._userState = (Http._userState st) { _sessionId = x } }

-- | Standard `IO` evaluator for `WDAct`.
evalWDAct :: WDAct a -> IO a
evalWDAct act = case act of
  ReadFilePath path -> try $ readFile path
  WriteFilePath path bytes -> try $ writeFile path bytes
  FileExists path -> try $ doesFileExist path

  HGetLine handle -> try $ do
    hFlush handle
    hGetLine handle

  HGetLineNoEcho handle -> try $ do
    hFlush handle
    echo <- hGetEcho handle
    hSetEcho handle False
    secret <- hGetLine handle
    hSetEcho handle echo
    return secret

instance Assert WebDriver where
  assert = log . LogAssertion

-- | Standard `Mock.MockIO` evaluator for `WDAct`.
evalWDActMockIO :: WDAct a -> Mock.MockIO u a
evalWDActMockIO act = case act of
  ReadFilePath path -> error "evalWDActMockIO: ReadFilePath not defined"

  WriteFilePath path bytes -> error "evalWDActMockIO: WriteFilePath not defined"

  FileExists path -> error "evalWDActMockIO: FileExists not defined"

  HGetLine handle -> error "evalWDActMockIO: HGetLine not defined"

  HGetLineNoEcho handle -> error "evalWDActMockIO: HGetLineNoEcho not defined"



-- | Initialize the configuration for a `WebDriver` shell session.
wdInit
  :: WebDriverConfig IO
  -> IO (IORef (WebDriverConfig IO))
wdInit config = newIORef config

-- | Interpret a step in a `WebDriver` shell session.
wdHttp
  :: IORef (WebDriverConfig IO)
  -> WebDriver a
  -> IO a
wdHttp ref wd = do
  config <- readIORef ref
  (result, st, _) <- execWebDriver config wd
  writeIORef ref config { _initialState = st }
  case result of
    Left e -> error $ Http.printE show e
    Right ok -> return ok

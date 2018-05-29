{- |
Module      : Web.Api.Http.Types
Description : Auxilliary types for the `HttpSession` monad.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

`HttpSession` is a stack of monads dealing with errors, state,environment, and logs. This module defines the auxiliary types used to represent this information.
-}

{-# LANGUAGE RecordWildCards #-}
module Web.Api.Http.Types (
  -- * Errors
    Err(..)
  , printErr

  -- * Logs
  , Log(..)
  , Entry(..)
  , HttpVerb(..)
  , SessionVerb(..)

  -- * State
  , St()
  , theTcpSession
  , setTcpSession
  , theLastResponse
  , setLastResponse
  , theClientState
  , setClientState
  , updateClientState
  , basicState

  -- *environment
  , Env()
  , theLogPrinter
  , setLogPrinter
  , theLogVerbosity
  , setLogVerbosity
  , theLogHandle
  , setLogHandle
  , theAssertionLogHandle
  , setAssertionLogHandle
  , theConsoleInHandle
  , setConsoleInHandle
  , theConsoleOutHandle
  , setConsoleOutHandle
  , theErrorInjectFunction
  , setErrorInjectFunction
  , theClientEnvironment
  , setClientEnvironment
  , theLogLock
  , setLogLock
  , theSessionUid
  , setSessionUid
  , basicEnv
  , jsonEnv

  -- * Configuration
  , HttpSessionConfig(..)
  , basicHttpSessionConfig
  , jsonHttpSessionConfig
  , setEnvironment
  , setState

  -- * Pretty Printing
  , LogPrinter(..)
  , printEntryWith
  , basicLogPrinter
  , jsonLogPrinter
  , LogVerbosity()
  , noisyLog
  , silentLog
  ) where

import Data.Aeson
  ( Value, decode )
import Control.Exception
  ( IOException )
import Data.Aeson.Lens
  ( _Value )
import Data.Aeson.Encode.Pretty
  ( encodePretty )
import Control.Lens
import Data.ByteString.Lazy.Char8
  ( unpack, pack )
import qualified Network.Wreq as Wreq
  ( Options, responseStatus )
import qualified Network.Wreq.Session as WreqS
  ( Session )
import Data.ByteString.Lazy
  ( ByteString, fromStrict )
import Data.Time
  ( UTCTime )
import Network.HTTP.Client
  ( HttpException(HttpExceptionRequest)
  , HttpExceptionContent(StatusCodeException) )
import System.IO
  ( Handle, stderr, stdout, stdin )
import System.IO.Error
  ( ioeGetFileName, ioeGetLocation, ioeGetErrorString )
import Control.Concurrent.MVar
  ( MVar )


import Web.Api.Http.Assert
import Web.Api.Http.Effects
import Web.Api.Http.Json



-- | Errors we expect to encounter during an HTTP interaction.

data Err err
  -- | HTTP Exceptions, like network errors and non-ok status responses.
  = ErrHttp HttpException

  -- | IO exceptions, like "disk full" errors.
  | ErrIO IOException

  -- | JSON parsing or decoding errors.
  | ErrJson JsonError

  -- | Expected failure, and success is catastrophic.
  | ErrUnexpectedSuccess String

  -- | For catastrophic failed assertions.
  | ErrUnexpectedFailure String

  -- | Unspecified error; defined by consumers of `HttpSession`.
  | Err err
  deriving Show


-- | For pretty printing.
printErr :: (err -> String) -> Err err -> String
printErr p e = case e of
  ErrHttp err -> show err
  ErrIO err -> show err
  ErrJson err -> show err
  ErrUnexpectedSuccess msg -> msg
  ErrUnexpectedFailure msg -> msg
  Err err -> p err



-- | The events we expect to log during an HTTP interaction. The `Log` type is really two logs rolled into one: there's the "session log", where we keep track of requests and responses and explicitly logged messages, and the "assertion log", where we keep track of `Assertion`s. Each log is just a list of timestamp/message pairs.

data Log err log = Log
  [(UTCTime, Entry err log)]
  [(UTCTime, Assertion)]

instance Monoid (Log err log) where
  mempty = Log [] []

  mappend (Log as1 bs1) (Log as2 bs2) =
    Log (as1 ++ as2) (bs1 ++ bs2)

-- | Default log entry constructors of different semantics. As with errors, each client may need to log other kinds of information, represented by the `log` type.

data Entry err log
  -- | Unspecified log entry type; defined by consumers of `HttpSession`.
  = LogItem log

  -- | Delays, measured in microseconds.
  | LogPause Int

  -- | Arbitrary text.
  | LogComment String

  -- | An assertion was made. This is independent of the assertion log.
  | LogAssertion Assertion

  -- | An HTTP request was made.
  | LogRequest HttpVerb Url Wreq.Options (Maybe ByteString)

  -- | An HTTP request was made, but the details are omitted. Useful for keeping secrets out of the logs.
  | LogSilentRequest

  -- | An HTTP response was received.
  | LogResponse HttpResponse

  -- | An HTTP response was received, but the details are omitted. Useful for keeping secrets out of the logs.
  | LogSilentResponse

  -- | A `Session` (in the @wreq@ sense) was opened or closed.
  | LogSession SessionVerb

  -- | An HTTP error was thrown.
  | LogHttpError HttpException

  -- | An IO error was thrown.
  | LogIOError IOException

  -- | A JSON error was thrown.
  | LogJsonError JsonError

  -- | Unexpected success.
  | LogUnexpectedSuccess String

  -- | Unexpected failure.
  | LogUnexpectedFailure String

  -- | An unspecified error of type defined by consumers of `HttpSession` was thrown.
  | LogError err
  deriving Show

-- | Representation of the HTTP request types.
data HttpVerb
  = DELETE | GET | POST
  deriving (Eq, Show)

-- | Representation of the actions we can perform on a `Session` (in the @wreq@ sense).
data SessionVerb
  = Close | Open
  deriving (Eq, Show)



-- | An opaque type representing the state we expect to keep during an HTTP interaction.

data St st = St {
  -- | If not `Nothing`, this allows the `HttpSession` to reuse TCP connections and remember cookies.
    _httpSession :: Maybe WreqS.Session

  -- | The "most recent" non-error HTTP response.
  , _lastResponse :: Maybe HttpResponse

  -- | Specified by consumers of `HttpSession`.
  , _userState :: st
  }

-- | Retrieve the TCP session in a monadic context.
theTcpSession
  :: (Monad m) => St st -> m (Maybe WreqS.Session)
theTcpSession = return . _httpSession

-- | Set the TCP session of an `St`.
setTcpSession
  :: Maybe WreqS.Session -> St st -> St st
setTcpSession s st = st { _httpSession = s }

-- | Retrieve the most recent HTTP response in a monadic context.
theLastResponse
  :: (Monad m) => St st -> m (Maybe HttpResponse)
theLastResponse = return . _lastResponse

-- | Set the most recent HTTP response of an `St`.
setLastResponse
  :: Maybe HttpResponse -> St st -> St st
setLastResponse r st = st { _lastResponse = r }

-- | Retrieve the client state in a monadic context.
theClientState
  :: (Monad m) => St st -> m st
theClientState = return . _userState

-- | Set the client state of an `St`.
setClientState
  :: st -> St st -> St st
setClientState s st = st { _userState = s }

-- | Mutate the client state of an `St`.
updateClientState
  :: (st -> st) -> St st -> St st
updateClientState f st = st
  { _userState = f $ _userState st }

-- | For creating an "initial" state, with all other fields left as `Nothing`s.
basicState :: st -> St st
basicState st = St
  { _httpSession = Nothing
  , _lastResponse = Nothing
  , _userState = st
  }




-- | An opaque type representing the read-onlyenvironment we expect to need during an `HttpSession`. This is for configuration-like state that doesn't change over the course of a single session, like the location of the log file.

data Env err log env = Env {
  -- | Used for making the appearance of logs configurable.
    _logPrinter :: LogPrinter err log

  -- | Used to control the verbosity of the logs.
  , _logVerbosity :: LogVerbosity err log

  -- | The handle of the session log file.
  , _logHandle :: Handle

  -- | The handle of the assertion log file.
  , _assertionLogHandle :: Handle

  -- | The handle of console input.
  , _consoleInHandle :: Handle

  -- | The handle of console output.
  , _consoleOutHandle :: Handle

  -- | A function used to inject `HttpException`s into the consumer-supplied error type. Handy when dealing with APIs that use HTTP error codes semantically.
  , _httpErrorInject :: Maybe (HttpException -> Maybe err)

  , _logLock :: Maybe (MVar ())

  , _sessionUid :: String

  -- | Unspecifiedenvironment type; defined by consumers of `HttpSession`.
  , _userEnv :: env
  }

-- | Retrieve the `LogPrinter` in a monadic context.
theLogPrinter
  :: (Monad m) => Env err log env -> m (LogPrinter err log)
theLogPrinter = return . _logPrinter

-- | Set the `LogPrinter` of an `Env`.
setLogPrinter
  :: LogPrinter err log -> Env err log env -> Env err log env
setLogPrinter printer env = env { _logPrinter = printer }

-- | Retrieve the `LogVerbosity` in a monadic context.
theLogVerbosity
  :: (Monad m) => Env err log env -> m (LogVerbosity err log)
theLogVerbosity = return . _logVerbosity

-- | Set the `LogVerbosity` of an `Env`.
setLogVerbosity
  :: LogVerbosity err log -> Env err log env -> Env err log env
setLogVerbosity verbosity env = env { _logVerbosity = verbosity }

-- | Retrieve the log handle in a monadic context.
theLogHandle
  :: (Monad m) => Env err log env -> m Handle
theLogHandle = return . _logHandle

-- | Set the log handle of an `Env`.
setLogHandle
  :: Handle -> Env err log env -> Env err log env
setLogHandle handle env = env { _logHandle = handle }

-- | Retrieve the assertion log handle in a monadic context.
theAssertionLogHandle
  :: (Monad m) => Env err log env -> m Handle
theAssertionLogHandle = return . _assertionLogHandle

-- | Set the assertion log handle of an `Env`.
setAssertionLogHandle
  :: Handle -> Env err log env -> Env err log env
setAssertionLogHandle handle env = env { _assertionLogHandle = handle }

-- | Retrieve the log handle in a monadic context.
theConsoleInHandle
  :: (Monad m) => Env err log env -> m Handle
theConsoleInHandle = return . _consoleInHandle

-- | Set the log handle of an `Env`.
setConsoleInHandle
  :: Handle -> Env err log env -> Env err log env
setConsoleInHandle handle env = env { _consoleInHandle = handle }

-- | Retrieve the log handle in a monadic context.
theConsoleOutHandle
  :: (Monad m) => Env err log env -> m Handle
theConsoleOutHandle = return . _consoleOutHandle

-- | Set the log handle of an `Env`.
setConsoleOutHandle
  :: Handle -> Env err log env -> Env err log env
setConsoleOutHandle handle env = env { _consoleOutHandle = handle }

-- | Retrieve the HTTP exception injecting function in a monadic context.
theErrorInjectFunction
  :: (Monad m) => Env err log env -> m (Maybe (HttpException -> Maybe err))
theErrorInjectFunction = return . _httpErrorInject

-- | Set the HTTP exception injecting function of an `Env`.
setErrorInjectFunction
  :: Maybe (HttpException -> Maybe err) -> Env err log env -> Env err log env
setErrorInjectFunction func env = env { _httpErrorInject = func }

-- | Retrieve the clientenvironment in a monadic context.
theClientEnvironment
  :: (Monad m) => Env err log env -> m env
theClientEnvironment = return . _userEnv

-- | Set the clientenvironment of an `Env`.
setClientEnvironment
  :: env -> Env err log env -> Env err log env
setClientEnvironment e env = env { _userEnv = e }

-- | Retrieve the log lock in a monadic context.
theLogLock
  :: (Monad m) => Env err log env -> m (Maybe (MVar ()))
theLogLock = return . _logLock

-- | Set the log lock of an `Env`.
setLogLock
  :: Maybe (MVar ()) -> Env err log env -> Env err log env
setLogLock logLock env = env { _logLock = logLock }

-- | Retrieve the log lock in a monadic context.
theSessionUid
  :: (Monad m) => Env err log env -> m String
theSessionUid = return . _sessionUid

-- | Set the log lock of an `Env`.
setSessionUid
  :: String -> Env err log env -> Env err log env
setSessionUid sessionUid env = env { _sessionUid = sessionUid }


-- | A reasonable standardenvironment for text or binary oriented APIs: logs are printed with `basicLogPrinter`, the session log goes to `stderr`, and the assertion log goes to `stdout`.
basicEnv
  :: (err -> String)                    -- ^ Function for printing consumer-defined errors.
  -> (log -> String)                    -- ^ Function for printing consumer-defined logs.
  -> Maybe (HttpException -> Maybe err) -- ^ Function for injecting HTTP exceptions as consumer-defined errors.
  -> env
  -> Env err log env
basicEnv printErr printLog promote env = Env
  { _logPrinter = basicLogPrinter True printErr printLog printAssertion
  , _logVerbosity = noisyLog
  , _logHandle = stderr
  , _assertionLogHandle = stdout
  , _consoleInHandle = stdin
  , _consoleOutHandle = stdout
  , _httpErrorInject = promote
  , _logLock = Nothing
  , _sessionUid = ""
  , _userEnv = env
  }

-- | A reasonable standardenvironment for JSON oriented APIs: logs are printed with `jsonLogPrinter`, the session log goes to `stderr`, and the assertion log goes to `stdout`.
jsonEnv
  :: (err -> String)                    -- ^ Function for printing consumer-defined errors.
  -> (log -> String)                    -- ^ Function for printing consumer-defined logs.
  -> Maybe (HttpException -> Maybe err) -- ^ Function for injecting HTTP exceptions as consumer-defined errors.
  -> env
  -> Env err log env
jsonEnv printErr printLog promote env = Env
  { _logPrinter = jsonLogPrinter True printErr printLog printAssertion
  , _logVerbosity = noisyLog
  , _logHandle = stderr
  , _assertionLogHandle = stdout
  , _consoleInHandle = stdin
  , _consoleOutHandle = stdout
  , _httpErrorInject = promote
  , _logLock = Nothing
  , _sessionUid = ""
  , _userEnv = env
  }




-- | Each HTTP session needs an initial state and an environment to run in; we wrap this data in a helper type called `HttpSessionConfig` for convenience. Doing this -- rather than passing all this into `runSession` directly -- makes session context explicitly first class, and also gives us more flexibility to add new default context in the future if needed.

data HttpSessionConfig err st log env = HttpSessionConfig
  { _initialState :: St st
  , _environment :: Env err log env
  }

-- We also give a couple of built in configurations. One assumes that all HTTP payloads are formatted as JSON (a reasonable and commonly valid assumption) and the other does not. Clients don't have to deviate from these unless they really want to.

-- | A reasonable standard configuration for text or binary oriented APIs. Uses `basicState` and `basicEnv` internally.
basicHttpSessionConfig
  :: (err -> String)                    -- ^ Function for printing consumer-defined errors.
  -> (log -> String)                    -- ^ Function for printing consumer-defined logs.
  -> Maybe (HttpException -> Maybe err) -- ^ Function for injecting HTTP exceptions as consumer-defined errors.
  -> st
  -> env
  -> HttpSessionConfig err st log env
basicHttpSessionConfig printErr printLog promote st env =
  HttpSessionConfig
    { _initialState = basicState st
    , _environment = basicEnv printErr printLog promote env
    }

-- | A reasonable standard configuration for JSON oriented APIs. Uses `basicState` and `jsonEnv` internally.
jsonHttpSessionConfig
  :: (err -> String)                    -- ^ Function for printing consumer-defined errors.
  -> (log -> String)                    -- ^ Function for printing consumer-defined logs.
  -> Maybe (HttpException -> Maybe err) -- ^ Function for injecting HTTP exceptions as consumer-defined errors.
  -> st
  -> env
  -> HttpSessionConfig err st log env
jsonHttpSessionConfig printErr printLog promote st env =
  HttpSessionConfig
    { _initialState = basicState st
    , _environment = jsonEnv printErr printLog promote env
    }

-- | Mutate the environment data of an `HttpSessionConfig`.
setEnvironment
  :: (Env err log env -> Env err log env)
  -> HttpSessionConfig err st log env
  -> HttpSessionConfig err st log env
setEnvironment f config = config
  { _environment = f $ _environment config }

-- | Mutate the initial state of an `HttpSessionConfig`.
setState
  :: (St st -> St st)
  -> HttpSessionConfig err st log env
  -> HttpSessionConfig err st log env
setState f config = config
  { _initialState = f $ _initialState config }


-- | A type representing the functions used to print logs. With this type, and the `basicLogPrinter` and `jsonLogPrinter` values, consumers of `HttpSession` don't have to think about formatting their logs beyond their specific `log` and `err` types unless they *really* want to.
data LogPrinter err log = LogPrinter {
  -- | Printer for consumer-defined errors.
    _error :: UTCTime -> String -> err -> String

  -- | Printer for comments.
  , _comment :: UTCTime -> String -> String -> String

  -- | Printer for detailed HTTP requests.
  , _request :: UTCTime -> String -> HttpVerb -> String -> Wreq.Options -> Maybe ByteString -> String

  -- | Printer for detailed HTTP responses.
  , _response :: UTCTime -> String -> HttpResponse -> String

  -- | Printer for `HttpException`s.
  , _httpError :: UTCTime -> String -> HttpException -> String

  -- | Printer for silent HTTP requests.
  , _silentRequest :: UTCTime -> String -> String

  -- | Printer for slient HTTP responses.
  , _silentResponse :: UTCTime -> String -> String

  -- | Printer for `IOException`s.
  , _ioError :: UTCTime -> String -> IOException -> String

  -- | Printer for session actions.
  , _session :: UTCTime -> String -> SessionVerb -> String

  -- | Printer for `JsonError`s.
  , _jsonError :: UTCTime -> String -> JsonError -> String

  -- | Printer for `UnexpectedSuccess`.
  , _unexpectedSuccess :: UTCTime -> String -> String -> String

  -- | Printer for `UnexpectedFailure`.
  , _unexpectedFailure :: UTCTime -> String -> String -> String

  -- | Printer for `Assertion`s.
  , _assertion :: UTCTime -> String -> Assertion -> String

  -- | Printer for delay events.
  , _pause :: UTCTime -> String -> Int -> String

  -- | Printer for consumer-defined log entries.
  , _log :: UTCTime -> String -> log -> String
  }


-- | Type representing fine-grained log verbosity options.
data LogVerbosity err log = LogVerbosity
  { _errVerbosity :: err -> Bool
  , _commentVerbosity :: Bool
  , _requestVerbosity :: Bool
  , _responseVerbosity :: Bool
  , _httpErrorVerbosity :: Bool
  , _silentRequestVerbosity :: Bool
  , _silentResponseVerbosity :: Bool
  , _ioErrorVerbosity :: Bool
  , _jsonErrorVerbosity :: Bool
  , _unexpectedSuccessVerbosity :: Bool
  , _unexpectedFailureVerbosity :: Bool
  , _sessionVerbosity :: Bool
  , _assertionVerbosity :: Bool
  , _pauseVerbosity :: Bool
  , _clientLogVerbosity :: log -> Bool
  }


-- | Prints all logs.
noisyLog :: LogVerbosity err log
noisyLog = LogVerbosity
  { _errVerbosity = const True
  , _commentVerbosity = True
  , _requestVerbosity = True
  , _responseVerbosity = True
  , _httpErrorVerbosity = True
  , _silentRequestVerbosity = True
  , _silentResponseVerbosity = True
  , _ioErrorVerbosity = True
  , _jsonErrorVerbosity = True
  , _unexpectedSuccessVerbosity = True
  , _unexpectedFailureVerbosity = True
  , _sessionVerbosity = True
  , _assertionVerbosity = True
  , _pauseVerbosity = True
  , _clientLogVerbosity = const True
  }


-- | Suppresses all logs.
silentLog :: LogVerbosity err log
silentLog = LogVerbosity
  { _errVerbosity = const False
  , _commentVerbosity = False
  , _requestVerbosity = False
  , _responseVerbosity = False
  , _httpErrorVerbosity = False
  , _silentRequestVerbosity = False
  , _silentResponseVerbosity = False
  , _ioErrorVerbosity = False
  , _jsonErrorVerbosity = False
  , _unexpectedSuccessVerbosity = False
  , _unexpectedFailureVerbosity = False
  , _sessionVerbosity = False
  , _assertionVerbosity = False
  , _pauseVerbosity = False
  , _clientLogVerbosity = const False
  }


-- | Prints a log entry with the given `LogPrinter`.
printEntryWith :: LogPrinter err log -> LogVerbosity err log -> (UTCTime, String, Entry err log) -> Maybe String
printEntryWith printer LogVerbosity{..} (time, uid, entry) = case entry of
  LogError err -> if _errVerbosity err
    then Just $ _error printer time uid err
    else Nothing
  LogComment msg -> if _commentVerbosity
    then Just $ _comment printer time uid msg
    else Nothing
  LogRequest verb url opts payload -> if _requestVerbosity
    then Just $ _request printer time uid verb url opts payload
    else Nothing
  LogResponse str -> if _responseVerbosity
    then Just $ _response printer time uid str
    else Nothing
  LogHttpError err -> if _httpErrorVerbosity
    then Just $ _httpError printer time uid err
    else Nothing
  LogSilentRequest -> if _silentRequestVerbosity
    then Just $ _silentRequest printer time uid
    else Nothing
  LogSilentResponse -> if _silentResponseVerbosity
    then Just $ _silentResponse printer time uid
    else Nothing
  LogIOError err -> if _ioErrorVerbosity
    then Just $ _ioError printer time uid err
    else Nothing
  LogJsonError err -> if _jsonErrorVerbosity
    then Just $ _jsonError printer time uid err
    else Nothing
  LogUnexpectedSuccess msg -> if _unexpectedSuccessVerbosity
    then Just $ _unexpectedSuccess printer time uid msg
    else Nothing
  LogUnexpectedFailure msg -> if _unexpectedFailureVerbosity
    then Just $ _unexpectedFailure printer time uid msg
    else Nothing
  LogSession verb -> if _sessionVerbosity
    then Just $ _session printer time uid verb
    else Nothing
  LogAssertion x -> if _assertionVerbosity
    then Just $ _assertion printer time uid x
    else Nothing
  LogPause m -> if _pauseVerbosity
    then Just $ _pause printer time uid m
    else Nothing
  LogItem x -> if _clientLogVerbosity x
    then Just $ _log printer time uid x
    else Nothing


-- Helpers for printing timestamps and coloring text.

trunc :: UTCTime -> String
trunc = take 19 . show

paint :: Bool -> (String -> String) -> String -> String
paint inColor f = if inColor then f else id

red str = "\x1b[1;31m" ++ str ++ "\x1b[0;39;49m"
blue str = "\x1b[1;34m" ++ str ++ "\x1b[0;39;49m"
green str = "\x1b[1;32m" ++ str ++ "\x1b[0;39;49m"
yellow str = "\x1b[1;33m" ++ str ++ "\x1b[0;39;49m"
magenta str = "\x1b[1;35m" ++ str ++ "\x1b[0;39;49m"


-- | A `LogPrinter` that treats all requests and responses as plain text.
basicLogPrinter
  :: Bool                   -- ^ If true, print in color
  -> (err -> String)        -- ^ Function for printing consumer-defined errors
  -> (log -> String)        -- ^ Function for printing consumer-defined logs
  -> (Assertion -> String)  -- ^ Function for printing `Assertion`s
  -> LogPrinter err log
basicLogPrinter color pE pL pA = LogPrinter
  { _error = \time uid err -> unwords
    [paint color red $ trunc time, uid, "ERROR", pE err]
  , _comment = \time uid msg -> unwords
    [paint color green $ trunc time, uid, msg]
  , _request = \time uid verb url opts payload -> unwords
    [paint color blue $ trunc time, uid, show verb, url]
  , _response = \time uid response -> unwords
    [paint color blue $ trunc time, uid, show response]
  , _httpError = \time uid err -> unwords
    [paint color red $ trunc time, uid, show err]
  , _silentRequest = \time uid -> unwords
    [paint color blue $ trunc time, uid, "Silent Request"]
  , _silentResponse = \time uid -> unwords
    [paint color blue $ trunc time, uid, "Silent Response"]
  , _ioError = \time uid err -> unwords
    [ paint color red $ trunc time, uid
    , show $ ioeGetFileName err
    , ioeGetLocation err
    , ioeGetErrorString err
    ]
  , _jsonError = \time uid err -> unwords
    [paint color red $ trunc time, uid, show err]
  , _unexpectedSuccess = \time uid msg -> unwords
    [paint color red $ trunc time, uid, "Unexpected Success: ", msg]
  , _unexpectedFailure = \time uid msg -> unwords
    [paint color red $ trunc time, uid, "Unexpected Failure: ", msg]
  , _session = \time uid verb -> unwords
    [paint color magenta $ trunc time, uid, show verb]
  , _assertion = \time uid a -> unwords
    [paint color magenta $ trunc time, uid, pA a]
  , _pause = \time uid m -> unwords
    [paint color magenta $ trunc time, uid, "pause for", show m ++ "μs"]
  , _log = \time uid x -> unwords
    [paint color yellow $ trunc time, uid, pL x]
  }

-- | `LogPrinter` that treats all requests and responses as JSON objects.
jsonLogPrinter
  :: Bool                  -- ^ If true, print in color
  -> (err -> String)       -- ^ Function for printing consumer-defined errors
  -> (log -> String)       -- ^ Function for printing consumer-defined logs
  -> (Assertion -> String) -- ^ Function for printing `Assertion`s
  -> LogPrinter err log
jsonLogPrinter color pE pL pA = (basicLogPrinter color pE pL pA)
  { _request = \time uid verb url opts payload ->
      let
        json = case payload of
          Nothing -> ""
          Just x -> case decode x of
            Nothing -> "parse error:\n" ++ unpack x
            Just v -> ('\n':) $ unpack $ encodePretty (v :: Value)
      in
        unwords [paint color blue $ trunc time, uid, show verb, url]
          ++ json
  , _response = \time uid response ->
      let
        headers = _responseHeaders response
        json = unpack $ encodePretty $ preview _Value $ _responseBody response
      in
        unwords [paint color blue $ trunc time, uid, "Response"]
          ++ "\n" ++ json
  , _httpError = \time uid err ->
      case triageHttpException err of
        Nothing -> unwords [paint color red $ trunc time, show err]
        Just (code,json) ->
          unwords [paint color red $ trunc time, uid, "HTTP Error Response", code]
            ++ "\n" ++ json
  }

triageHttpException :: HttpException -> Maybe (String, String)
triageHttpException e = case e of
  HttpExceptionRequest _ (StatusCodeException s r) -> do
    json <- decode $ fromStrict r
    let status = s ^. Wreq.responseStatus 
    return (show status, unpack $ encodePretty (json :: Value))
  _ -> Nothing

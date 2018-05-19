{- |
Module      : Web.Api.WebDriver.Monad
Description : A WebDriver session monad.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

The `WebDriver` type is a specialization of `HttpSession` with additional context specific to the WebDriver spec.
-}

{-# LANGUAGE OverloadedStrings #-}
module Web.Api.WebDriver.Monad (
    WebDriver
  , WebDriverConfig

  -- * Configuration
  , webDriverConfig
  , defaultWebDriverConfig

  -- * WebDriver State
  , WebDriverState()
  , defaultWebDriverState
  , theSessionId
  , setSessionId

  -- * WebDriver Environment
  , WebDriverEnv()
  , ResponseFormat(..)
  , ApiVersion(..)
  , defaultWebDriverEnv
  , theRemoteHostname
  , setRemoteHostname
  , theRemotePort
  , setRemotePort
  , theRemotePath
  , setRemotePath
  , theSecretsPath
  , setSecretsPath
  , theApiVersion
  , setApiVersion
  , theVars
  , setVars
  , theResponseFormat
  , setResponseFormat
  , readResponseFormat

  -- * WebDriver Errors
  , WebDriverError(..)
  , printWebDriverError
  , expect

  -- * Helpers
  , theRemoteUrl
  , theRemoteUrlWithSession

  -- * Shell
  , wdshInit
  ) where

import Control.Lens
  ( (^.), (^?) )
import Data.Void
  ( Void )
import qualified Data.Map.Strict as M
import Data.Aeson
  ( Value(), Result(Success), toJSON, (.=), fromJSON
  , object )
import Data.Aeson.Lens
  ( key, _Value, _String )
import Data.Aeson.Encode.Pretty
  ( encodePretty )
import Data.IORef
  ( IORef )
import Data.Text
  ( pack, unpack, Text )
import qualified Data.ByteString.Lazy as LB
  ( ByteString, unpack )
import qualified Data.ByteString.Lazy.Char8 as LC
  ( unpack, pack )
import qualified Data.ByteString.Char8 as SC
  ( unpack )
import Network.Wreq
  ( Status, statusMessage, statusCode, responseStatus )
import qualified Network.HTTP.Client as N

import Web.Api.Http
import Web.Api.WebDriver.Types



-- | Synonym for `HttpSession`.
type WebDriver m a =
  HttpSession m WebDriverError WebDriverState Void WebDriverEnv a

-- | Synonym for `HttpSessionConfig`.
type WebDriverConfig =
  HttpSessionConfig WebDriverError WebDriverState Void WebDriverEnv



-- | Constructor for `WebDriverConfig`s with default printers.
webDriverConfig :: WebDriverState -> WebDriverEnv -> WebDriverConfig
webDriverConfig st env = jsonHttpSessionConfig
  printWebDriverError show (Just promoteHttpResponseError) st env

-- | Default config.
defaultWebDriverConfig :: WebDriverConfig
defaultWebDriverConfig =
  webDriverConfig defaultWebDriverState defaultWebDriverEnv



-- | Includes a @Maybe String@ representing the current session ID, if one has been opened.
data WebDriverState = WebDriverState
  { __session_id :: Maybe String
  } deriving Show

-- | Session ID set to `Nothing`.
defaultWebDriverState :: WebDriverState
defaultWebDriverState = WebDriverState
  { __session_id = Nothing
  }

-- | Retrieve the session id in a monadic context.
theSessionId :: (Monad m) => WebDriverState -> m (Maybe String)
theSessionId = return . __session_id

-- | Set the session id of a `WebDriverState`.
setSessionId :: Maybe String -> WebDriverState -> WebDriverState
setSessionId x st = st { __session_id = x }



-- | Read-only environment variables specific to WebDriver.
data WebDriverEnv = WebDriverEnv
  { __remote_hostname :: String -- ^ Hostname of the remote WebDriver server
  , __remote_port :: Int -- ^ Port of the remote WebDriver server
  , __remote_path :: String -- ^ Extra path for the remote WebDriver server
  , __secrets_path :: FilePath -- ^ Path where secret data is stored
  , __vars :: M.Map String String -- ^ Named constants; this makes it possible to e.g. run the same WebDriver session against both a test and production environment on different hosts.
  , __response_format :: ResponseFormat -- ^ Flag for the format of HTTP responses from the remote end. E.g., chromedriver reponses are not spec-compliant.
  , __api_version :: ApiVersion -- ^ Version of the WebDriver specification.
  }

-- | Configured for geckodriver.
defaultWebDriverEnv :: WebDriverEnv
defaultWebDriverEnv = WebDriverEnv
  { __remote_hostname = "localhost"
  , __remote_port = 4444
  , __remote_path = ""
  , __secrets_path = ""
  , __vars = M.empty
  , __response_format = SpecFormat
  , __api_version = CR_2018_03_04
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


-- | Retrieve the remote hostname in a monadic context.
theRemoteHostname :: (Monad m) => WebDriverEnv -> m String
theRemoteHostname = return . __remote_hostname

-- | Set the remote hostname of a `WebDriverEnv`.
setRemoteHostname :: String -> WebDriverEnv -> WebDriverEnv
setRemoteHostname host env = env { __remote_hostname = host }

-- | Retrieve the remote port in a monadic context.
theRemotePort :: (Monad m) => WebDriverEnv -> m Int
theRemotePort = return . __remote_port

-- | Set the remote port of a `WebDriverEnv`.
setRemotePort :: Int -> WebDriverEnv -> WebDriverEnv
setRemotePort port env = env { __remote_port = port }

-- | Retrieve the remote path in a monadic context.
theRemotePath :: (Monad m) => WebDriverEnv -> m String
theRemotePath = return . __remote_path

-- | Set the remote path of a `WebDriverEnv`.
setRemotePath :: String -> WebDriverEnv -> WebDriverEnv
setRemotePath path env = env { __remote_path = path }

-- | Retrieve the secrets path in a monadic context.
theSecretsPath :: (Monad m) => WebDriverEnv -> m FilePath
theSecretsPath = return . __secrets_path

-- | Set the secrets of a `WebDriverEnv`.
setSecretsPath :: FilePath -> WebDriverEnv -> WebDriverEnv
setSecretsPath path env = env { __secrets_path = path }

-- | Retrieve the WebDriver API version in a monadic context.
theApiVersion :: (Monad m) => WebDriverEnv -> m ApiVersion
theApiVersion = return . __api_version

-- | Set the API version of a `WebDriverEnv`.
setApiVersion :: ApiVersion -> WebDriverEnv -> WebDriverEnv
setApiVersion version env = env { __api_version = version }

-- | Retrieve the vars map.
theVars :: (Monad m) => WebDriverEnv -> m (M.Map String String)
theVars = return . __vars

-- | Set the vars map of a `WebDriverEnv`.
setVars :: M.Map String String -> WebDriverEnv -> WebDriverEnv
setVars vars env = env { __vars = vars }

-- | Retrieve the response format in a monadic context.
theResponseFormat :: (Monad m) => WebDriverEnv -> m ResponseFormat
theResponseFormat = return . __response_format

-- | Set the response format of a `WebDriverEnv`.
setResponseFormat :: ResponseFormat -> WebDriverEnv -> WebDriverEnv
setResponseFormat format env = env { __response_format = format }

-- | Retrieve the `ResponseFormat` in a `WebDriver` context directly.
readResponseFormat
  :: (Effectful m)
  => WebDriver m ResponseFormat
readResponseFormat =
  getEnvironment
    >>= theClientEnvironment
    >>= theResponseFormat



-- | Errors specific to WebDriver sessions.
data WebDriverError
  = NoSession
  | ResponseError ResponseErrorCode String String (Maybe Value) Status -- ^ See <https://w3c.github.io/webdriver/webdriver-spec.html#handling-errors>
  | UnableToConnect
  | UnhandledHttpException N.HttpException
  | ImageDecodeError String
  | UnexpectedValue String
  deriving (Show)

-- | For validating responses. Throws an `UnexpectedValue` error if the two arguments are not equal according to their `Eq` instance.
expect :: (Eq a, Show a, Effectful m) => a -> a -> WebDriver m a
expect x y = if x == y
  then return y
  else throwError $ Err $ UnexpectedValue $
    "expected " ++ show x ++ " but got " ++ show y

-- | Promote semantic HTTP exceptions to typed errors.
promoteHttpResponseError :: N.HttpException -> Maybe WebDriverError
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

  _ -> Just $ UnhandledHttpException e

-- | For pretty printing.
printWebDriverError :: WebDriverError -> String
printWebDriverError e = case e of
  NoSession -> "No session in progress"
  ResponseError code msg trace obj status ->
    let
      code = status ^. statusCode
      smsg = status ^. statusMessage
    in
      (("Response: " ++ show code ++ " " ++ SC.unpack smsg ++ "\n") ++) $
      LC.unpack $ encodePretty $ object
        [ "error" .= (toJSON code)
        , "message" .= (toJSON msg)
        , "stacktrace" .= (toJSON trace)
        , "data" .= (toJSON <$> obj)
        ]
  UnableToConnect -> "Unable to connect to WebDriver server"
  UnhandledHttpException e -> "Unhandled HTTP Exception: " ++ show e
  ImageDecodeError msg -> "Image decode: " ++ msg
  UnexpectedValue msg -> "Unexpected value: " ++ msg




-- | Url of the remote WebDriver server.
theRemoteUrl :: (Effectful m) => WebDriver m String
theRemoteUrl = do
  host <- getEnvironment >>= theClientEnvironment >>= theRemoteHostname
  port <- getEnvironment >>= theClientEnvironment >>= theRemotePort
  path <- getEnvironment >>= theClientEnvironment >>= theRemotePath
  return $ concat [ "http://", host, ":", show port, path]

-- | Url of the remote WebDriver server, with session ID.
theRemoteUrlWithSession
  :: (Effectful m)
  => WebDriver m String
theRemoteUrlWithSession = do
  st <- getState >>= theClientState >>= theSessionId
  case st of
    Nothing -> throwError $ Err NoSession
    Just session_id -> do
      baseUrl <- theRemoteUrl
      return $ concat [ baseUrl, "/session/", session_id ]



-- | Initialize state for a WebDriver shell session.
wdshInit
  :: WebDriverConfig
  -> IO (IORef (St WebDriverState, Env WebDriverError Void WebDriverEnv))
wdshInit config = initShell (__initial_state config) (__environment config)

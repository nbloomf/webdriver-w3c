{- |
Module      : Web.Api.Http.Helpers
Description : Helper functions for the `HttpSession` monad.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

module Web.Api.Http.Helpers (
  -- * Threads
    pauseInMicroseconds

  -- * IO
  , promptForString
  , promptForSecret
  , readFilePath
  , writeFilePath

  -- * HTTP Requests
  , httpGet
  , httpSilentGet
  , httpPost
  , httpSilentPost
  , httpDelete
  , httpSilentDelete

  -- * TCP Sessions
  , openTcpSession
  , closeTcpSession
  ) where

import Data.ByteString.Lazy (ByteString)
import qualified Network.Wreq as Wreq (defaults, Options)
import qualified Network.Wreq.Session as WreqS (Session)
import Network.HTTP.Client (HttpException)

import Web.Api.Http.Types
import Web.Api.Http.Effects
import Web.Api.Http.Monad


-- | Pause execution for /at least/ the given number of microseconds.
pauseInMicroseconds
  :: (Effectful m)
  => Int -> HttpSession m err st log env ()
pauseInMicroseconds m =
  logNow (LogPause m) >> mThreadDelay m

-- | Prompt for input on @stdin@.
promptForString
  :: (Effectful m)
  => String -- ^ Prompt text
  -> m String
promptForString prompt =
  mPutStrLn prompt >> mReadLine

-- | Prompt for input on @stdin@, but do not echo the typed characters back to the screen - handy for getting suuper secret info.
promptForSecret
  :: (Effectful m)
  => String -- ^ Prompt text
  -> m String
promptForSecret prompt =
  mPutStrLn prompt >> mReadLineNoEcho

-- | Read a file as a `ByteString`, capturing any IO errors in `HttpSession`.
readFilePath
  :: (Effectful m)
  => FilePath
  -> HttpSession m err st log env ByteString
readFilePath path = do
  result <- mTry $ mReadFile path
  case result of
    Right contents -> return contents
    Left e -> do
      logNow $ LogIOError e
      throwError $ ErrIO e

-- | Write a `ByteString` to a file, capturing any IO errors in `HttpSession`.
writeFilePath
  :: (Effectful m)
  => FilePath
  -> ByteString
  -> HttpSession m err st log env ()
writeFilePath path contents = do
  result <- liftSession $ mTry $ mWriteFile path contents
  case result of
    Right () -> return ()
    Left e -> do
      logNow $ LogIOError e
      throwError $ ErrIO e



-- | Opens a session (in the @wreq@ sense)
openTcpSession
  :: (Effectful m)
  => HttpSession m err st log env ()
openTcpSession = do
  session <- mNewSessionState
  logNow $ LogSession Open
  state <- getState
  putState $ setTcpSession (Just session) state

-- | Closes a session (in the @wreq@ sense)
closeTcpSession
  :: (Effectful m)
  => HttpSession m err st log env ()
closeTcpSession = do
  logNow $ LogSession Close
  state <- getState
  putState $ setTcpSession Nothing state



data RequestOptions = RequestOptions
  { _http_options :: Wreq.Options
  , _log_request :: Bool
  , _log_response :: Bool
  }

defaultRequestOptions :: RequestOptions
defaultRequestOptions = RequestOptions
  { _http_options = Wreq.defaults
  , _log_request = True
  , _log_response = True
  }

silentRequestOptions :: RequestOptions
silentRequestOptions = RequestOptions
  { _http_options = Wreq.defaults
  , _log_request = False
  , _log_response = False
  }

-- | The basic shape of all requests is the same, so we abstract it here for consistency.
processRequest
  :: (Effectful m)
  => RequestOptions
  -> Entry err log
  -> (Maybe WreqS.Session -> m (Either HttpException HttpResponse))
  -> HttpSession m err st log env HttpResponse

processRequest opt entry request = do
  let options = _http_options opt

  -- Log the request (or not).
  if _log_request opt
   then logNow entry
   else logNow LogSilentRequest

  -- Run one of two requests, depending on
  -- whether we have an active session.
  session <- getState >>= theTcpSession
  result <- liftSession $ request session

  case result of
    -- If the result was ok:
    Right response -> do
      -- Log the response (or not).
      logNow $ if _log_response opt
        then LogResponse response
        else LogSilentResponse

      -- Remember the response in state.
      state <- getState
      putState $ setLastResponse (Just response) state

      return response

    -- If there was an HTTP exception:
    Left err -> do
      w <- captureHttpError err
      case w of
        Just z -> do
          logNow $ if _log_response opt
            then LogError z
            else LogSilentResponse
          throwError $ Err z
        Nothing -> do
          -- Log it (or not)
          logNow $ if _log_response opt
            then LogHttpError err
            else LogSilentResponse
          throwError $ ErrHttp err



httpGetWith
  :: (Effectful m)
  => RequestOptions
  -> Url
  -> HttpSession m err st log env HttpResponse
httpGetWith opt url = processRequest opt
  (LogRequest GET url (_http_options opt) Nothing)
  (\sn -> mGetWith (_http_options opt) sn url)

-- | Send a @GET@ request to a url.
httpGet
  :: (Effectful m)
  => Url -> HttpSession m err st log env HttpResponse
httpGet = httpGetWith defaultRequestOptions

-- | Send a @GET@ request to a url, without logging either the request or the response.
httpSilentGet
  :: (Effectful m)
  => Url -> HttpSession m err st log env HttpResponse
httpSilentGet = httpGetWith silentRequestOptions



httpPostWith
  :: (Effectful m)
  => RequestOptions
  -> Url
  -> ByteString -- ^ Payload
  -> HttpSession m err st log env HttpResponse
httpPostWith opt url payload = processRequest opt
  (LogRequest POST url (_http_options opt) (Just payload))
  (\sn -> mPostWith (_http_options opt) sn url payload)

-- | Send a @POST@ request with a given payload to a url.
httpPost
  :: (Effectful m)
  => Url
  -> ByteString -- ^ Payload
  -> HttpSession m err st log env HttpResponse
httpPost = httpPostWith defaultRequestOptions

-- | Send a @POST@ request with a given payload to a url, without logging either the request or the response.
httpSilentPost
  :: (Effectful m)
  => Url
  -> ByteString -- ^ Payload
  -> HttpSession m err st log env HttpResponse
httpSilentPost = httpPostWith silentRequestOptions



httpDeleteWith
  :: (Effectful m)
  => RequestOptions
  -> Url
  -> HttpSession m err st log env HttpResponse
httpDeleteWith opt url = processRequest opt
  (LogRequest DELETE url (_http_options opt) Nothing)
  (\sn -> mDeleteWith (_http_options opt) sn url)

-- | Send a @DELETE@ request to a url.
httpDelete
  :: (Effectful m)
  => Url
  -> HttpSession m err st log env HttpResponse
httpDelete = httpDeleteWith defaultRequestOptions

-- | Send a @DELETE@ request to a url without logging either the request or the response.
httpSilentDelete
  :: (Effectful m)
  => Url
  -> HttpSession m err st log env HttpResponse
httpSilentDelete = httpDeleteWith silentRequestOptions

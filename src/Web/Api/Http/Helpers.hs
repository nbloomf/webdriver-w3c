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

import qualified Data.ByteString.Lazy as LB
  ( ByteString )
import qualified Network.Wreq as Wreq
  ( defaults, Options )
import qualified Network.Wreq.Session as WreqS
  ( Session )
import Network.HTTP.Client
  ( HttpException )

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
  :: (EffectConsole m)
  => String -- ^ Prompt text
  -> m String
promptForString prompt =
  mPutStrLn prompt >> mGetLine

-- | Prompt for input on @stdin@, but do not echo the typed characters back to the screen - handy for getting suuper secret info.
promptForSecret
  :: (EffectConsole m)
  => String -- ^ Prompt text
  -> m String
promptForSecret prompt =
  mPutStrLn prompt >> mGetLineNoEcho

-- | Read a file as a `ByteString`, capturing any IO errors in `HttpSession`.
readFilePath
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m)
  => FilePath
  -> HttpSession m err st log env LB.ByteString
readFilePath path = do
  result <- mTry $ mReadFile path
  case result of
    Right contents -> return contents
    Left e -> do
      logNow $ LogIOError e
      throwError $ ErrIO e

-- | Write a `ByteString` to a file, capturing any IO errors in `HttpSession`.
writeFilePath
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m)
  => FilePath
  -> LB.ByteString
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
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m, EffectHttp m)
  => HttpSession m err st log env ()
openTcpSession = do
  session <- mNewSessionState
  logNow $ LogSession Open
  state <- getState
  putState $ setTcpSession (Just session) state

-- | Closes a session (in the @wreq@ sense)
closeTcpSession
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m, EffectHttp m)
  => HttpSession m err st log env ()
closeTcpSession = do
  logNow $ LogSession Close
  state <- getState
  putState $ setTcpSession Nothing state



data RequestOptions = RequestOptions
  { _httpOptions :: Wreq.Options
  , _logRequest :: Bool
  , _logResponse :: Bool
  }

defaultRequestOptions :: RequestOptions
defaultRequestOptions = RequestOptions
  { _httpOptions = Wreq.defaults
  , _logRequest = True
  , _logResponse = True
  }

silentRequestOptions :: RequestOptions
silentRequestOptions = RequestOptions
  { _httpOptions = Wreq.defaults
  , _logRequest = False
  , _logResponse = False
  }

-- | The basic shape of all requests is the same, so we abstract it here for consistency.
processRequest
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m, EffectHttp m)
  => RequestOptions
  -> Entry err log
  -> (Maybe WreqS.Session -> m (Either HttpException HttpResponse))
  -> HttpSession m err st log env HttpResponse

processRequest opt entry request = do
  let options = _httpOptions opt

  -- Log the request (or not).
  if _logRequest opt
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
      logNow $ if _logResponse opt
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
          logNow $ if _logResponse opt
            then LogError z
            else LogSilentResponse
          throwError $ Err z
        Nothing -> do
          -- Log it (or not)
          logNow $ if _logResponse opt
            then LogHttpError err
            else LogSilentResponse
          throwError $ ErrHttp err



httpGetWith
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m, EffectHttp m)
  => RequestOptions
  -> Url
  -> HttpSession m err st log env HttpResponse
httpGetWith opt url = processRequest opt
  (LogRequest GET url (_httpOptions opt) Nothing)
  (\sn -> mGetWith (_httpOptions opt) sn url)

-- | Send a @GET@ request to a url.
httpGet
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m, EffectHttp m)
  => Url -> HttpSession m err st log env HttpResponse
httpGet = httpGetWith defaultRequestOptions

-- | Send a @GET@ request to a url, without logging either the request or the response.
httpSilentGet
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m, EffectHttp m)
  => Url -> HttpSession m err st log env HttpResponse
httpSilentGet = httpGetWith silentRequestOptions



httpPostWith
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m, EffectHttp m)
  => RequestOptions
  -> Url
  -> LB.ByteString -- ^ Payload
  -> HttpSession m err st log env HttpResponse
httpPostWith opt url payload = processRequest opt
  (LogRequest POST url (_httpOptions opt) (Just payload))
  (\sn -> mPostWith (_httpOptions opt) sn url payload)

-- | Send a @POST@ request with a given payload to a url.
httpPost
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m, EffectHttp m)
  => Url
  -> LB.ByteString -- ^ Payload
  -> HttpSession m err st log env HttpResponse
httpPost = httpPostWith defaultRequestOptions

-- | Send a @POST@ request with a given payload to a url, without logging either the request or the response.
httpSilentPost
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m, EffectHttp m)
  => Url
  -> LB.ByteString -- ^ Payload
  -> HttpSession m err st log env HttpResponse
httpSilentPost = httpPostWith silentRequestOptions



httpDeleteWith
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m, EffectHttp m)
  => RequestOptions
  -> Url
  -> HttpSession m err st log env HttpResponse
httpDeleteWith opt url = processRequest opt
  (LogRequest DELETE url (_httpOptions opt) Nothing)
  (\sn -> mDeleteWith (_httpOptions opt) sn url)

-- | Send a @DELETE@ request to a url.
httpDelete
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m, EffectHttp m)
  => Url
  -> HttpSession m err st log env HttpResponse
httpDelete = httpDeleteWith defaultRequestOptions

-- | Send a @DELETE@ request to a url without logging either the request or the response.
httpSilentDelete
  :: (EffectTry m, EffectTimer m, EffectFiles m, EffectConsole m, EffectHttp m)
  => Url
  -> HttpSession m err st log env HttpResponse
httpSilentDelete = httpDeleteWith silentRequestOptions

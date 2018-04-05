{- |
Module      : Web.Api.Http.Monad
Description : An HTTP session monad, with batteries included.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

In this module we define the `HttpSession` type and its helpers. Together these are a set of parts for building HTTP clients, which are themselves little languages for describing HTTP sessions at a high level.

From a bird's eye view, an "HTTP session" is a sequence of requests from a client and responses from a server. But there's more: in addition to this sequence of requests and responses being communicated with the outside world, there are several other issues an HTTP client might need to worry about.

* Detecting and handling *errors* of many kinds, such as HTTP error responses.
* Maintaining an internal mutable *state*, such as a set of cookies.
* Keeping a write-only *log* of the session.
* Dealing with a read-only *environment*, such as a set of configuration options.

The log and environment are both kinds of state, but with access restrictions -- during the course of a session, we can write to the log, but not read it, and we can read from the environment, but not change it. We can think of the mutable state, the read-only environment, and the write-only log collectively as the *context* within which a session takes place.

Monads are great for modeling complex sequential computations like this in a principled way.

We can think of an HTTP session as a *function* taking a context, doing some communication over the network (maybe), and then returning either some value or an error, as well as an updated context. That is, a function with a signature like this:

    @context -> IO (Either error value, context)@

The actual definition of our `HttpSession` type is just barely more complicated than that. But we don't use the definition directly; instead we build and execute `HttpSessions` using the monad interface.
-}

{-# LANGUAGE DeriveDataTypeable #-}
module Web.Api.Http.Monad
  ( HttpSession(execSession)
  , runSession
  , debugSession
  , liftSession

  -- * Environment
  , getEnvironment

  -- * State
  , getState
  , putState

  -- * Logs
  , logNow
  , comment
  , logger
  , assertNow

  -- * Errors
  , throwError
  , catchError
  , captureHttpError
) where

import Data.Time (UTCTime)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Aeson.Lens as AL (_Value)
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch (MonadThrow(..))
import qualified Control.Lens as L (preview)
import Network.HTTP.Client (HttpException)

import Web.Api.Http.Effects
import Web.Api.Http.Assert
import Web.Api.Http.Json
import Web.Api.Http.Types


-- | An opaque type representing abstract HTTP interactions. The @err@, @log@, @st@, and @env@ types are escape hatches for allowing consumers of this library to extend the error handling and state that come built in. @m@ is where all the IO-like effects happen; in practice it's the real `IO`, but we can swap it for a fake IO for testing.
newtype HttpSession m err st log env t = HttpSession
  { execSession ::
      (St st, Log err log, Env err log env)
        -> m (Either (Err err) t, (St st, Log err log))
  } deriving Typeable


-- | Run an `HttpSession`, returning either a value (@a@) or an error (@Err err@).
runSession
  :: (Monad m)
  => HttpSessionConfig err st log env
  -> HttpSession m err st log env a
  -> m (Either (Err err) a)
runSession config session = do
  let state = __initial_state config
  let env = __environment config
  (result, _) <- execSession session (state, mempty, env)
  return result

-- | Run an `HttpSession`, returning the list of assertions it makes.
debugSession
  :: (Monad m)
  => HttpSessionConfig err st log env
  -> HttpSession m err st log env a
  -> m (Either (Err err) a, [Assertion])
debugSession config session = do
  let state = __initial_state config
  let env = __environment config
  (result, (_, (Log _ assertions))) <- execSession session (state, mempty, env)
  return (result, map snd assertions)



instance (Monad m) => Monad (HttpSession m err st log env) where
  return x = HttpSession $ \(st, log, env) ->
    return (Right x, (st, log))

  x >>= f = HttpSession $ \(st, log, env) -> do
    (result, (st1, log1)) <- execSession x (st, log, env)
    case result of
      Right ok -> execSession (f ok) (st1, log1, env)
      Left err -> return (Left err, (st1, log1))

instance (Monad m) => Functor (HttpSession m err st log env) where
  fmap f x = x >>= (return . f)

instance (Monad m) => Applicative (HttpSession m err st log env) where
  pure = return
  (<*>) = ap

-- | Injects an @m a@ into an HTTP session.
liftSession :: (Functor m) => m a -> HttpSession m err st log env a
liftSession x = HttpSession $ \(st,log,_) ->
  fmap (\a -> (Right a, (st,log))) x

instance (MonadIO m) => MonadIO (HttpSession m err st log env) where
  liftIO = liftSession . liftIO

instance (MonadIO m) => MonadThrow (HttpSession m err st log env) where
  throwM = liftIO . throwM



-- | Retrieve the current environment.
getEnvironment
  :: (Monad m)
  => HttpSession m err st log env (Env err log env)
getEnvironment = HttpSession $ \(st, log, env) ->
  return (Right env, (st, log))



-- | Retrieve the current state.
getState
  :: (Monad m)
  => HttpSession m err st log env (St st)
getState = HttpSession $ \(st, log, _) -> 
  return (Right st, (st, log))

-- | Overwrite the current state.
putState
  :: (Monad m)
  => St st
  -> HttpSession m err st log env ()
putState st = HttpSession $ \(_, log, _) ->
  return (Right (), (st, log))




-- | Append a log to the current log.
appendLog
  :: (Monad m)
  => Log err log
  -> (HttpSession m err st log env ())
appendLog msg = HttpSession $ \(state, log, _) ->
  return (Right (), (state, mappend log msg))

-- | Write an entry to the log.
logNow
  :: (EffectTimer m, EffectConsole m)
  => Entry err log
  -> HttpSession m err st log env ()
logNow msg = do
  time <- mGetSystemTime
  printer <- getEnvironment >>= theLogPrinter
  handle <- getEnvironment >>= theLogHandle
  mhPutStrLn handle $ printEntryWith printer (time, msg)
  mhFlush handle
  appendLog $ Log [(time, msg)] []

-- | Write a comment to the log.
comment
  :: (Effectful m)
  => String
  -> HttpSession m err st log env ()
comment = logNow . LogComment

-- | Write a consumer-defined entry value to the log.
logger
  :: (Effectful m)
  => log
  -> HttpSession m err st log env ()
logger = logNow . LogItem

-- | Write an `Assertion` to the assertion log.
assertNow
  :: (Effectful m)
  => Assertion
  -> HttpSession m err st log env ()
assertNow a = do
  logNow $ LogAssertion a
  time <- mGetSystemTime
  if isSuccess a
    then return ()
    else do
      printer <- getEnvironment >>= theLogPrinter
      handle <- getEnvironment >>= theAssertionLogHandle
      mhPutStrLn handle $ printEntryWith printer (time, LogAssertion a)
      mhFlush handle
  appendLog $ Log [] [(time, a)]



-- | Throw an error.
throwError
  :: (Monad m, EffectTimer m, EffectConsole m)
  => Err err
  -> HttpSession m err st log env a
throwError e = do
  case e of
    ErrHttp e -> logNow $ LogHttpError e
    ErrIO e -> logNow $ LogIOError e
    ErrJson e -> logNow $ LogJsonError e
    ErrUnexpectedSuccess m -> logNow $ LogUnexpectedSuccess m
    ErrUnexpectedFailure m -> logNow $ LogUnexpectedFailure m
    Err e -> logNow $ LogError e
  HttpSession $ \(st, log, _) ->
    return (Left e, (st, log))

-- | Attempt an `HttpSession` computation, but capture errors with the given handler.
catchError
  :: (Monad m, Effectful m)
  => HttpSession m err st log env a
  -> (Err err -> HttpSession m err st log env a) -- ^ Error handler
  -> HttpSession m err st log env a
catchError session handler = HttpSession $ \(st0,log0,env) -> do
  (result,(st1,log1)) <- execSession session (st0,log0,env)
  case result of
    Right ok -> return (Right ok, (st1,log1))
    Left err -> execSession (handler err) (st1,log1,env)

-- | Inject HTTP exceptions to our native consumer-defined error type using the injection function defined in `Env`.
captureHttpError
  :: (Monad m)
  => HttpException
  -> HttpSession m err st log env (Maybe err)
captureHttpError e = do
  w <- getEnvironment >>= theErrorInjectFunction
  case w of
    Just inject -> return (inject e)
    Nothing -> return Nothing



instance (Effectful m) => Assert (HttpSession m err st log env) where
  assert = assertNow

instance (Monad m, Effectful m) => Json (HttpSession m err st log env) where
  mRaiseJsonError e = throwError (ErrJson e)

instance (EffectConsole m) => EffectConsole (HttpSession m err st log env) where
  mhGetEcho handle = liftSession $ mhGetEcho handle
  mhSetEcho handle bool = liftSession $ mhSetEcho handle bool

  mStdIn = getEnvironment >>= theConsoleInHandle
  mhGetChar handle = liftSession $ mhGetChar handle
  mhGetLine handle = liftSession $ mhGetLine handle

  mStdOut = getEnvironment >>= theConsoleOutHandle
  mhFlush handle = liftSession $ mhFlush handle
  mhPutChar handle c = liftSession $ mhPutChar handle c
  mhPutStr handle str = liftSession $ mhPutStr handle str
  mhPutStrLn handle str = liftSession $ mhPutStrLn handle str

instance (EffectTimer m) => EffectTimer (HttpSession m err st log env) where
  mGetSystemTime = liftSession mGetSystemTime
  mThreadDelay k = liftSession $ mThreadDelay k

instance (EffectTry m) => EffectTry (HttpSession m err st log env) where
  mTry (HttpSession f) = HttpSession $ \(st,log,env) -> do
    result <- mTry $ f (st,log,env)
    case result of
      Left err -> return (Right (Left err), (st,log))
      Right (it,(st2,log2)) -> case it of
        Left err -> return (Left err, (st2,log2))
        Right ok -> return (Right (Right ok), (st2,log2))

instance (EffectFiles m) => EffectFiles (HttpSession m err st log env) where
  mReadFile path = liftSession $ mReadFile path
  mWriteFile path bytes = liftSession $ mWriteFile path bytes
  mFileExists path = liftSession $ mFileExists path

instance (EffectRandom m) => EffectRandom (HttpSession m err st log env) where
  mRandom = liftSession mRandom
  mRandomBetween (lo,hi) = liftSession $ mRandomBetween (lo,hi)

instance (EffectHttp m) => EffectHttp (HttpSession m err st log env) where
  mGetWith opts sesh url = liftSession $ mGetWith opts sesh url
  mPostWith opts sesh url bytes = liftSession $ mPostWith opts sesh url bytes
  mDeleteWith opts sesh url = liftSession $ mDeleteWith opts sesh url
  mNewSessionState = liftSession mNewSessionState

{- |
Module      : Web.Api.Http.Effects
Description : Typeclasses representing IO effects. Used for mocking.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

Typeclasses representing IO effects. By writing our effectful code against the classes, we can swap out the real 'IO' for a fake IO for testing purposes.

Each class represents a cluster of similar effects.

* `EffectPrint`: Writing to files
* `EffectConsole`: Reading and writing to the terminal
* `EffectTimer`: Getting the current time, suspending the current thread
* `EffectTry`: Throwing exceptions
* `EffectFiles`: Interacting with the filesystem
* `EffectRandom`: Generating random values
* `EffectHttp`: HTTP requests
-}

module Web.Api.Http.Effects (
    Effectful

  -- * Writing to Files
  , EffectPrint(..)

  -- * Read and write from the terminal
  , EffectConsole(..)

  -- * Time
  , EffectTimer(..)
  , mPauseInSeconds

  -- * Try
  , EffectTry(..)

  -- * Filesystem
  , EffectFiles(..)

  -- * Randomness
  , EffectRandom(..)
  , mRandomDecimalDigit
  , mRandomLowerCaseLetter
  , mRandomUpperCaseLetter
  , mRandomAlphanumericCharacter
  , mRandomVec
  , mRandomDecimalDigitString
  , mRandomAlphanumericString

  -- * HTTP Requests
  , EffectHttp(..)
  , Url
  , HttpResponse(..)
  , readHttpResponse
  ) where

import Prelude hiding (try, readFile, writeFile)

import Control.Concurrent
  ( threadDelay )
import Control.Exception
  ( Exception, try )
import Data.Time as T
  ( UTCTime )
import Data.Time.Clock.System
  ( getSystemTime, systemToUTCTime )
import Data.ByteString.Lazy
  ( ByteString, readFile, writeFile )
import Network.Wreq
  ( Options, getWith, postWith, deleteWith )
import qualified Network.Wreq.Session as S
  ( Session, newSession, getWith, postWith, deleteWith )
import System.IO
  ( Handle, hPutStrLn, stdin, hGetEcho, hSetEcho, hFlush
  , hFlush, stdout, putChar, getLine )
import System.Directory
  ( doesFileExist )
import System.Random
import Network.HTTP.Client
  ( Response, CookieJar, responseCookieJar, responseBody
  , responseHeaders, responseVersion, responseStatus, HttpException )
import Network.HTTP.Types (Status, ResponseHeaders, HttpVersion)



-- | Monads that can print strings to file handles.

class (Monad m) => EffectPrint m where
  -- | Acts like `hPutStrLn`.
  mPrintLine :: Handle -> String -> m ()

  -- | Acts like `hFlush`.
  mFlush :: Handle -> m ()


instance EffectPrint IO where
  mPrintLine = hPutStrLn
  mFlush = hFlush


-- | Monads that can read from and write to a terminal.

class (Monad m) => EffectConsole m where
  -- | Acts like `putStr`.
  mPutStr :: String -> m ()

  -- | Acts like `putStrLn`.
  mPutStrLn :: String -> m ()

  -- | Flush `stdout`, then `getLine`.
  mReadLine :: m String

  -- | Flush `stdout`, then `getLine`, but do not echo typed characters to the terminal. For reading suuuper secret infos.
  mReadLineNoEcho :: m String


instance EffectConsole IO where
  mPutStr = putStr
  mPutStrLn = putStrLn

  mReadLine = hFlush stdout >> getLine

  mReadLineNoEcho = do
    hFlush stdout
    oldSetting <- hGetEcho stdin
    hSetEcho stdin False
    secret <- getLine
    hSetEcho stdin oldSetting
    putChar '\n'
    return secret



-- | Monads that can read the "current" time and pause execution.

class (Monad m) => EffectTimer m where
  -- | Acts like `getSystemTime`.
  mGetSystemTime :: m UTCTime

  -- | Acts like `threadDelay`.
  mThreadDelay :: Int -> m ()


instance EffectTimer IO where
  mGetSystemTime = fmap systemToUTCTime getSystemTime
  mThreadDelay = threadDelay


-- | Stop the current thread for at least `Int` seconds.
mPauseInSeconds :: (EffectTimer m) => Int -> m ()
mPauseInSeconds k = mThreadDelay (k * 1000000)




-- | Monads that can throw arbitrary exceptions.

class (Monad m) => EffectTry m where
  -- | Acts like `try`.
  mTry :: (Exception e) => m a -> m (Either e a)


instance EffectTry IO where
  mTry = try



-- | Monads that can interact with the filesystem.

class (Monad m) => EffectFiles m where
  -- | Acts like `readFile`.
  mReadFile :: FilePath -> m ByteString

  -- | Acts like `writeFile`.
  mWriteFile :: FilePath -> ByteString -> m ()

  -- | Acts like `doesFileExist`.
  mFileExists :: FilePath -> m Bool


instance EffectFiles IO where
  mReadFile = readFile
  mWriteFile = writeFile
  mFileExists = doesFileExist



-- | Monads that can generate random values.

class (Monad m) => EffectRandom m where
  -- | Acts like `randomIO`; generate a random `a`.
  mRandom :: (Random a) => m a

  -- | Acts like `randomRIO`; generate a random `a` in a given range.
  mRandomBetween :: (Random a) => (a,a) -> m a


instance EffectRandom IO where
  mRandom = randomIO
  mRandomBetween = randomRIO


-- | Random character in @['0'..'9']@.
mRandomDecimalDigit :: (EffectRandom m) => m Char
mRandomDecimalDigit = do
  i <- mRandomBetween (0,9)
  return $ ['0'..'9'] !! i

-- | Random character in @['a'..'z']@.
mRandomLowerCaseLetter :: (EffectRandom m) => m Char
mRandomLowerCaseLetter = do
  i <- mRandomBetween (0,25)
  return $ ['a'..'z'] !! i

-- | Random character in @['A'..'Z']@.
mRandomUpperCaseLetter :: (EffectRandom m) => m Char
mRandomUpperCaseLetter = do
  i <- mRandomBetween (0,25)
  return $ ['A'..'Z'] !! i

-- | Random character in @['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']@.
mRandomAlphanumericCharacter :: (EffectRandom m) => m Char
mRandomAlphanumericCharacter = do
  i <- mRandomBetween (0,61)
  return $ (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']) !! i

-- | Random list of length @Int@.
mRandomVec :: (EffectRandom m, Random a) => Int -> m a -> m [a]
mRandomVec length gen = mapM (const gen) [1..length]

-- | Random string of @Int@ digits.
mRandomDecimalDigitString :: (EffectRandom m) => Int -> m String
mRandomDecimalDigitString length =
  mRandomVec length mRandomDecimalDigit

-- | Random string of @Int@ alphanumeric characters.
mRandomAlphanumericString :: (EffectRandom m) => Int -> m String
mRandomAlphanumericString k =
  mRandomVec k mRandomAlphanumericCharacter



-- | Monads that can perform HTTP requests.
class (Monad m) => EffectHttp m where
  -- | Acts like `getWith` (no session) or `S.getWith` (session) from @wreq@.
  mGetWith :: Options -> Maybe S.Session -> Url -> m (Either HttpException HttpResponse)

  -- | Acts like `postWith` (no session) or `S.postWith` (session) from @wreq@.
  mPostWith :: Options -> Maybe S.Session -> Url -> ByteString -> m (Either HttpException HttpResponse)

  -- | Acts like `deleteWith` (no session) or `S.deleteWith` (session) from @wreq@.
  mDeleteWith :: Options -> Maybe S.Session -> Url -> m (Either HttpException HttpResponse)

  -- | Acts like `S.newSession` from @wreq@.
  mNewSessionState :: m S.Session

-- | Synonym for better type signatures.
type Url = String

instance EffectHttp IO where
  mGetWith opts session url = case session of
    Nothing -> mTry $ fmap readHttpResponse $ getWith opts url
    Just sn -> mTry $ fmap readHttpResponse $ S.getWith opts sn url

  mPostWith opts session url payload = case session of
    Nothing -> mTry $ fmap readHttpResponse $ postWith opts url payload
    Just sn -> mTry $ fmap readHttpResponse $ S.postWith opts sn url payload

  mDeleteWith opts session url = case session of
    Nothing -> mTry $ fmap readHttpResponse $ deleteWith opts url
    Just sn -> mTry $ fmap readHttpResponse $ S.deleteWith opts sn url

  mNewSessionState = S.newSession

-- | Our `IO` implementation is based on the `wreq` library, which can handle reusing TCP connections and maintaining cookies for us. Unfortunately the `Response` type that `wreq` deals with by default is opaque -- it is awkward to construct values of that type, say, for mocking a server. So instead we will extract `wreq`s responses to our own type, `HttpResponse`, that we can control.

data HttpResponse = HttpResponse
  { __response_status :: Status
  , __response_version :: HttpVersion
  , __response_headers :: ResponseHeaders
  , __response_body :: ByteString
  , __response_cookie_jar :: CookieJar
  } deriving Show

-- | Convert a `Response ByteString` into an `HttpResponse`.
readHttpResponse :: Response ByteString -> HttpResponse
readHttpResponse r = HttpResponse
  { __response_status = responseStatus r
  , __response_version = responseVersion r
  , __response_headers = responseHeaders r
  , __response_body = responseBody r
  , __response_cookie_jar = responseCookieJar r
  }



-- | Impose all of the effect classes with a single constraint.

class
  ( Monad m
  , EffectPrint m
  , EffectConsole m
  , EffectTimer m
  , EffectTry m
  , EffectFiles m
  , EffectRandom m
  , EffectHttp m
  ) => Effectful m

instance Effectful IO

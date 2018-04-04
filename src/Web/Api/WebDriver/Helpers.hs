{- |
Module      : Web.Api.WebDriver.Helpers
Description : Higher level WebDriver utilities.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

module Web.Api.WebDriver.Helpers (
  -- * Running Sessions
    cleanupOnError
  , runIsolated

  -- * Secrets
  , stashCookies
  , loadCookies
  , writeCookieFile
  , readCookieFile

  -- * Actions
  , press
  , typeString

  -- * Shell
  , openSession
  , closeSession
  ) where

import qualified Data.Aeson as Aeson
  ( encode )
import qualified Data.ByteString.Lazy.Char8 as BS
  ( pack )
import qualified Data.Digest.Pure.SHA as SHA
  ( showDigest, sha1 )

import Web.Api.Http
import Web.Api.WebDriver.Monad
import Web.Api.WebDriver.Types
import Web.Api.WebDriver.Types.Keyboard
import Web.Api.WebDriver.Endpoints



-- | If a WebDriver session ends without issuing a delete session command, then the server keeps its session state alive. `cleanupOnError` catches errors and ensures that a `deleteSession` request is sent.
cleanupOnError
  :: (Effectful m)
  => WebDriver m a -- ^ `WebDriver` session that may throw errors
  -> WebDriver m a
cleanupOnError x = catchError x (\e -> deleteSession >> throwError e)


-- | Run a WebDriver computation in an isolated browser session. Uses `cleanupOnError` to ensure the session is deleted on the server.
runIsolated
  :: (Effectful m)
  => Capabilities
  -> WebDriver m a
  -> WebDriver m ()
runIsolated caps theSession = cleanupOnError $ do
  session_id <- newSession caps
  getState >>= (putState . updateClientState (setSessionId (Just session_id)))
  theSession
  deleteSession
  getState >>= (putState . updateClientState (setSessionId Nothing))


-- | Save all cookies for the current domain to a file.
stashCookies
  :: (Effectful m)
  => String -- ^ Passed through SHA1, and the digest is used as the filename.
  -> WebDriver m ()
stashCookies string =
  let file = SHA.showDigest $ SHA.sha1 $ BS.pack string in
  getAllCookies >>= writeCookieFile file


-- | Load cookies from a file saved with `stashCookies`. Returns `False` if the cookie file is missing or cannot be read.
loadCookies
  :: (Effectful m)
  => String -- ^ Passed through SHA1, and the digest is used as the filename.
  -> WebDriver m Bool
loadCookies string = do
  let file = SHA.showDigest $ SHA.sha1 $ BS.pack string
  contents <- readCookieFile file
  case contents of
    Nothing -> return False
    Just cs -> do
      mapM addCookie cs
      return True


-- | Write cookies to a file under the secrets path. 
writeCookieFile
  :: (Effectful m)
  => FilePath -- ^ File path; relative to @$SECRETS_PATH/cookies/@
  -> [Cookie]
  -> WebDriver m ()
writeCookieFile file cookies = do
  path <- getEnvironment >>= theClientEnvironment >>= theSecretsPath
  let fullpath = path ++ "/cookies/" ++ file
  writeFilePath fullpath (Aeson.encode cookies)


-- | Read cookies from a file stored with `writeCookieFile`. Returns `Nothing` if the file does not exist.
readCookieFile
  :: (Effectful m)
  => FilePath -- ^ File path; relative to @$SECRETS_PATH/cookies/@
  -> WebDriver m (Maybe [Cookie])
readCookieFile file = do
  path <- getEnvironment >>= theClientEnvironment >>= theSecretsPath
  let fullpath = path ++ "/cookies/" ++ file
  cookieFileExists <- mFileExists fullpath
  if cookieFileExists
    then readFilePath fullpath
      >>= mParseJson
      >>= constructFromJSON
      >>= mapM constructFromJSON
      >>= (return . Just)
    else return Nothing


-- | `KeyDownAction` with the given `Char`.
keypress :: Char -> ActionItem
keypress x = emptyActionItem
  { _action_type = Just KeyDownAction
  , _action_value = Just [x]
  }


-- | Simulate pressing a `Key`.
press :: Key -> Action
press key = emptyAction
  { _input_source_type = Just KeyInputSource
  , _input_source_id = Just "kbd"
  , _action_items = [keypress (keyToChar key)]
  }


-- | Simulate typing some text.
typeString :: String -> Action
typeString x = emptyAction
  { _input_source_type = Just KeyInputSource
  , _input_source_id = Just "kbd"
  , _action_items = map keypress x
  }



-- | Open a new browser instance; for use with `httpShell`
openSession
  :: (Effectful m)
  => Capabilities
  -> WebDriver m ()
openSession caps = do
  session_id <- newSession caps
  getState >>= (putState . updateClientState (setSessionId (Just session_id)))
  return ()

-- | Close the current browser instance; for use with `httpShell`.
closeSession
  :: (Effectful m)
  => WebDriver m ()
closeSession = do
  deleteSession
  getState >>= (putState . updateClientState (setSessionId Nothing))
  return ()

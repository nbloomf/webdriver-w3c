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

import Web.Api.WebDriver.Endpoints
import Web.Api.WebDriver.Monad
import Web.Api.WebDriver.Types
import Web.Api.WebDriver.Types.Keyboard



-- | If a WebDriver session ends without issuing a delete session command, then the server keeps its session state alive. `cleanupOnError` catches errors and ensures that a `deleteSession` request is sent.
cleanupOnError
  :: WebDriver a -- ^ `WebDriver` session that may throw errors
  -> WebDriver a
cleanupOnError x = catchError x $ \e ->
  deleteSession >> throwError e



-- | Run a WebDriver computation in an isolated browser session. Uses `cleanupOnError` to ensure the session is deleted on the server.
runIsolated
  :: Capabilities
  -> WebDriver a
  -> WebDriver ()
runIsolated caps theSession = cleanupOnError $ do
  session_id <- newSession caps
  modifyState $ setSessionId (Just session_id)
  theSession
  deleteSession
  modifyState $ setSessionId Nothing


-- | Save all cookies for the current domain to a file.
stashCookies
  :: String -- ^ Passed through SHA1, and the digest is used as the filename.
  -> WebDriver ()
stashCookies string =
  let file = SHA.showDigest $ SHA.sha1 $ BS.pack string in
  getAllCookies >>= writeCookieFile file


-- | Load cookies from a file saved with `stashCookies`. Returns `False` if the cookie file is missing or cannot be read.
loadCookies
  :: String -- ^ Passed through SHA1, and the digest is used as the filename.
  -> WebDriver Bool
loadCookies string = do
  let file = SHA.showDigest $ SHA.sha1 $ BS.pack string
  contents <- readCookieFile file
  case contents of
    Nothing -> return False
    Just cs -> do
      mapM_ addCookie cs
      return True


-- | Write cookies to a file under the secrets path. 
writeCookieFile
  :: FilePath -- ^ File path; relative to @$SECRETS_PATH/cookies/@
  -> [Cookie]
  -> WebDriver ()
writeCookieFile file cookies = do
  path <- fromEnv (_secretsPath . _env)
  let fullpath = path ++ "/cookies/" ++ file
  writeFilePath fullpath (Aeson.encode cookies)


-- | Read cookies from a file stored with `writeCookieFile`. Returns `Nothing` if the file does not exist.
readCookieFile
  :: FilePath -- ^ File path; relative to @$SECRETS_PATH/cookies/@
  -> WebDriver (Maybe [Cookie])
readCookieFile file = do
  path <- fromEnv (_secretsPath . _env)
  let fullpath = path ++ "/cookies/" ++ file
  cookieFileExists <- fileExists fullpath
  if cookieFileExists
    then readFilePath fullpath
      >>= parseJson
      >>= constructFromJson
      >>= mapM constructFromJson
      >>= (return . Just)
    else return Nothing


-- | `KeyDownAction` with the given `Char`.
keypress :: Char -> ActionItem
keypress x = emptyActionItem
  { _actionType = Just KeyDownAction
  , _actionValue = Just [x]
  }


-- | Simulate pressing a `Key`.
press :: Key -> Action
press key = emptyAction
  { _inputSourceType = Just KeyInputSource
  , _inputSourceId = Just "kbd"
  , _actionItems = [keypress (keyToChar key)]
  }


-- | Simulate typing some text.
typeString :: String -> Action
typeString x = emptyAction
  { _inputSourceType = Just KeyInputSource
  , _inputSourceId = Just "kbd"
  , _actionItems = map keypress x
  }



-- | Open a new browser instance; for use with `httpShell`
openSession
  :: Capabilities
  -> WebDriver ()
openSession caps = do
  session_id <- newSession caps
  modifyState $ setSessionId (Just session_id)
  return ()

-- | Close the current browser instance; for use with `httpShell`.
closeSession
  :: WebDriver ()
closeSession = do
  deleteSession
  modifyState $ setSessionId Nothing
  return ()

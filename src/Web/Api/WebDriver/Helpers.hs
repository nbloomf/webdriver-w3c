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
  -- * Secrets
    stashCookies
  , loadCookies

  -- * Actions
  , press
  , typeString
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





-- | Save all cookies for the current domain to a file.
stashCookies
  :: (Monad m)
  => String -- ^ Passed through SHA1, and the digest is used as the filename.
  -> WebDriverT m ()
stashCookies string =
  let file = SHA.showDigest $ SHA.sha1 $ BS.pack string in
  getAllCookies >>= writeCookieFile file


-- | Load cookies from a file saved with `stashCookies`. Returns `False` if the cookie file is missing or cannot be read.
loadCookies
  :: (Monad m)
  => String -- ^ Passed through SHA1, and the digest is used as the filename.
  -> WebDriverT m Bool
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
  :: (Monad m)
  => FilePath -- ^ File path; relative to @$DATA_PATH\/secrets\/cookies\/@
  -> [Cookie]
  -> WebDriverT m ()
writeCookieFile file cookies = do
  path <- fromEnv (_dataPath . _env)
  let fullpath = path ++ "/secrets/cookies/" ++ file
  writeFilePath fullpath (Aeson.encode cookies)


-- | Read cookies from a file stored with `writeCookieFile`. Returns `Nothing` if the file does not exist.
readCookieFile
  :: (Monad m)
  => FilePath -- ^ File path; relative to @$DATA_PATH\/secrets\/cookies\/@
  -> WebDriverT m (Maybe [Cookie])
readCookieFile file = do
  path <- fromEnv (_dataPath . _env)
  let fullpath = path ++ "/secrets/cookies/" ++ file
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

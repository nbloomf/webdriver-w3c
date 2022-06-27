{- |
Module      : Web.Api.WebDriver.Helpers
Description : Higher level WebDriver utilities.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}
module Web.Api.WebDriver.Helpers (
  -- * Data
    writeDataFile
  , readDataFile
  , writeJsonFile
  , readJsonFile

  -- * Secrets
  , stashCookies
  , loadCookies

  -- * Actions
  , press
  , typeString
  ) where

import Control.Monad.Trans.Class
  ( MonadTrans(..) )
import qualified Data.Aeson as Aeson
  ( encode, ToJSON(..), Value )
import Data.ByteString.Lazy
  ( ByteString, fromChunks )
import qualified Data.ByteString.Lazy.Char8 as BS
  ( pack )
import qualified Data.Digest.Pure.SHA as SHA
  ( showDigest, sha1 )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Web.Api.WebDriver.Endpoints
import Web.Api.WebDriver.Monad
import Web.Api.WebDriver.Types
import Web.Api.WebDriver.Types.Keyboard





-- | Save all cookies for the current domain to a file.
stashCookies
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Text -- ^ Passed through SHA1, and the digest is used as the filename.
  -> WebDriverTT t eff ()
stashCookies string =
  let file = SHA.showDigest $ SHA.sha1 $ fromChunks [T.encodeUtf8 string] in
  getAllCookies >>= writeCookieFile file


-- | Load cookies from a file saved with `stashCookies`. Returns `False` if the cookie file is missing or cannot be read.
loadCookies
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Text -- ^ Passed through SHA1, and the digest is used as the filename.
  -> WebDriverTT t eff Bool
loadCookies string = do
  let file = SHA.showDigest $ SHA.sha1 $ fromChunks [T.encodeUtf8 string]
  contents <- readCookieFile file
  case contents of
    Nothing -> return False
    Just cs -> do
      mapM_ addCookie cs
      return True


-- | Write cookies to a file under the secrets path. 
writeCookieFile
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => FilePath -- ^ File path; relative to @$DATA_PATH\/secrets\/cookies\/@
  -> [Cookie]
  -> WebDriverTT t eff ()
writeCookieFile file cookies = do
  path <- fromEnv (_dataPath . _env)
  let fullpath = path ++ "/secrets/cookies/" ++ file
  writeFilePath fullpath (Aeson.encode cookies)


-- | Read cookies from a file stored with `writeCookieFile`. Returns `Nothing` if the file does not exist.
readCookieFile
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => FilePath -- ^ File path; relative to @$DATA_PATH\/secrets\/cookies\/@
  -> WebDriverTT t eff (Maybe [Cookie])
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



-- | Write a `ByteString` to the data directory
writeDataFile
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => FilePath -- ^ File path, relative to @$DATA_PATH@, with leading slash
  -> ByteString
  -> WebDriverTT t eff ()
writeDataFile file contents = do
  path <- fromEnv (_dataPath . _env)
  writeFilePath (path ++ file) contents

-- | Read a `ByteString` from the data directory
readDataFile
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => FilePath -- ^ File path, relative to @$DATA_PATH@, with leading slash
  -> WebDriverTT t eff ByteString
readDataFile file = do
  path <- fromEnv (_dataPath . _env)
  readFilePath $ path ++ file



-- | Write JSON to the data directory
writeJsonFile
  :: (Monad eff, Monad (t eff), MonadTrans t, Aeson.ToJSON a)
  => FilePath -- ^ File path, relative to @$DATA_PATH@, with leading slash
  -> a
  -> WebDriverTT t eff ()
writeJsonFile file a = do
  path <- fromEnv (_dataPath . _env)
  writeFilePath (path ++ file) (Aeson.encode $ Aeson.toJSON a)

-- | Read a JSON `Value` from the data directory
readJsonFile
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => FilePath -- ^ File path, relative to @$DATA_PATH@, with leading slash
  -> WebDriverTT t eff Aeson.Value
readJsonFile file = do
  path <- fromEnv (_dataPath . _env)
  readFilePath (path ++ file) >>= parseJson



-- | `KeyDownAction` with the given `Char`.
keypress :: Char -> ActionItem
keypress x = emptyActionItem
  { _actionType = Just KeyDownAction
  , _actionValue = Just $ T.singleton x
  }


-- | Simulate pressing a `Key`.
press :: Key -> Action
press key = emptyAction
  { _inputSourceType = Just KeyInputSource
  , _inputSourceId = Just "kbd"
  , _actionItems = [keypress (keyToChar key)]
  }


-- | Simulate typing some text.
typeString :: Text -> Action
typeString x = emptyAction
  { _inputSourceType = Just KeyInputSource
  , _inputSourceId = Just "kbd"
  , _actionItems = map keypress $ T.unpack x
  }

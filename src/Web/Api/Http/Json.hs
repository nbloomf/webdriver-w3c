{- |
Module      : Web.Api.Http.Json
Description : Helper functions for working with JSON.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

In practice lots of HTTP APIs deal in JSON-encoded data. This module includes some helpers for parsing and decoding this data.
-}

module Web.Api.Http.Json
  ( Json(..)
  , JsonError(..)

  , mParseJson
  , lookupKey
  , constructFromJSON
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Lens as AL (_Value)
import qualified Data.HashMap.Strict as HM (lookup)
import qualified Data.Vector as V (toList)
import qualified Data.Text as T (Text)
import qualified Data.ByteString.Lazy as LB (ByteString)
import qualified Control.Lens as L (preview)

-- | Represents computations that can handle JSON parsing and decoding errors.
class Json m where
  mRaiseJsonError :: JsonError -> m a

-- | Represents the kinds of errors that can occur when parsing and decoding JSON.
data JsonError
  -- | A generic JSON error; try not to use this.
  = JsonError

  -- | A failed parse.
  | JsonParseError

  -- | An attempt to look up the value of a key that does not exist on an object.
  | JsonKeyDoesNotExist T.Text

  -- | An attempt to look up the value of a key on something other than an object.
  | JsonKeyLookupOffObject T.Text

  -- | A failed attempt to convert a `Value` to some other type.
  | JsonConstructError String
  deriving (Eq, Show)


-- | Decode a `LB.ByteString` to an `A.Value`.
mParseJson :: (Monad m, Json m) => LB.ByteString -> m A.Value
mParseJson bytes = case L.preview AL._Value bytes of
  Just value -> return value
  Nothing -> mRaiseJsonError JsonParseError


-- | Object member lookup.
lookupKey :: (Monad m, Json m) => T.Text -> A.Value -> m A.Value
lookupKey key (A.Object obj) = case HM.lookup key obj of
  Nothing -> mRaiseJsonError (JsonKeyDoesNotExist key)
  Just value -> return value
lookupKey key _ = mRaiseJsonError (JsonKeyLookupOffObject key)


-- | Decode a `A.Value` to some other type.
constructFromJSON :: (Monad m, Json m, A.FromJSON a) => A.Value -> m a
constructFromJSON value = case A.fromJSON value of
  A.Success x -> return x
  A.Error msg -> mRaiseJsonError (JsonConstructError msg)

{- |
Module      : Web.Api.WebDriver.Uri
Description : Types and functions for validating parts of a URI.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

module Web.Api.WebDriver.Uri (
    Host()
  , mkHost
  , Port()
  , mkPort
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
  ( Arbitrary(..), oneof, vectorOf, Positive(..) )


-- | The host part of a URI. See <https://tools.ietf.org/html/rfc3986#page-18>.
newtype Host = Host
  { unHost :: Text
  } deriving Eq

-- | Constructor for hosts that checks for invalid characters.
mkHost :: Text -> Maybe Host
mkHost str =
  if T.all (`elem` hostAllowedChars) str
    then Just (Host str)
    else Nothing

instance Show Host where
  show = T.unpack . unHost

instance Arbitrary Host where
  arbitrary = do
    Positive k <- arbitrary
    str <- vectorOf k $ oneof $ map return hostAllowedChars
    case mkHost $ T.pack str of
      Just h -> return h
      Nothing -> error "In Arbitrary instance for Host: bad characters."

hostAllowedChars :: [Char]
hostAllowedChars = concat
  [ ['a'..'z'], ['A'..'Z'], ['0'..'9'], ['-','_','.','~','%'] ]



-- | The port part of a URI.
newtype Port = Port { unPort :: Text }
  deriving Eq

-- | Constructor for ports.
mkPort :: Text -> Maybe Port
mkPort str =
  if T.all (`elem` ['0'..'9']) str
    then Just (Port str)
    else Nothing

instance Show Port where
  show = T.unpack . unPort

instance Arbitrary Port where
  arbitrary = do
    Positive k <- arbitrary
    str <- vectorOf k $ oneof $ map return ['0'..'9']
    case mkPort $ T.pack str of
      Just p -> return p
      Nothing -> error "In Arbitrary instance for Port: bad characters."

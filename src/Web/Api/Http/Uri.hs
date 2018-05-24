{- |
Module      : Web.Api.Http.Uri
Description : Types and functions for validating parts of a URI.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

module Web.Api.Http.Uri (
    Host()
  , mkHost
  , Port()
  , mkPort
  ) where

import Test.QuickCheck
  ( Arbitrary(..), oneof, vectorOf, Positive(..) )


-- | The host part of a URI. See <https://tools.ietf.org/html/rfc3986#page-18>.
newtype Host = Host
  { unHost :: String
  } deriving Eq

-- | Constructor for hosts that checks for invalid characters.
mkHost :: String -> Maybe Host
mkHost str =
  if all (`elem` host_allowed_chars) str
    then Just (Host str)
    else Nothing

instance Show Host where
  show = unHost

instance Arbitrary Host where
  arbitrary = do
    Positive k <- arbitrary
    str <- vectorOf k $ oneof $ map return host_allowed_chars
    case mkHost str of
      Just h -> return h
      Nothing -> error "In Arbitrary instance for Host: bad characters."

host_allowed_chars :: [Char]
host_allowed_chars = concat
  [ ['a'..'z'], ['A'..'Z'], ['0'..'9'], ['-','_','.','~','%'] ]



-- | The port part of a URI.
newtype Port = Port { unPort :: String }
  deriving Eq

-- | Constructor for ports.
mkPort :: String -> Maybe Port
mkPort str =
  if all (`elem` ['0'..'9']) str
    then Just (Port str)
    else Nothing

instance Show Port where
  show = unPort

instance Arbitrary Port where
  arbitrary = do
    Positive k <- arbitrary
    str <- vectorOf k $ oneof $ map return ['0'..'9']
    case mkPort str of
      Just p -> return p
      Nothing -> error "In Arbitrary instance for Port: bad characters."

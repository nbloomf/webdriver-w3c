{- |
Module      : Test.Tasty.WebDriver.Config
Description : Helpers for parsing config files.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE RecordWildCards #-}
module Test.Tasty.WebDriver.Config (
    DriverName(..)
  , RemoteEndPool(..)
  , addRemoteEndForDriver
  , getRemoteEndForDriver
  , RemoteEnd(..)

  -- * Parsing
  , parseRemoteEnd
  , parseRemoteEndConfig
  , parseRemoteEndOption
  ) where

import Network.URI
import Text.Read (readMaybe)
import Data.List (unlines, isPrefixOf, isSuffixOf, nub)

import Data.Typeable
  ( Typeable, Proxy(Proxy) )
import qualified Data.Map.Strict as MS



-- | Remote end name.
data DriverName
  = Geckodriver
  | Chromedriver
  deriving (Eq, Ord, Typeable)

instance Show DriverName where
  show Geckodriver = "geckodriver"
  show Chromedriver = "chromedriver"


-- | Pool of remote end connections per driver.
newtype RemoteEndPool = RemoteEndPool
  { freeRemoteEnds :: MS.Map DriverName [RemoteEnd]
  } deriving (Eq, Show)

instance Monoid RemoteEndPool where
  mempty = RemoteEndPool
    { freeRemoteEnds = MS.fromList []
    }

  mappend x y = RemoteEndPool
    { freeRemoteEnds = MS.unionWith (++) (freeRemoteEnds x) (freeRemoteEnds y)
    }

-- | Push a remote end to the pool stack for a given driver.
addRemoteEndForDriver :: DriverName -> RemoteEnd -> RemoteEndPool -> RemoteEndPool
addRemoteEndForDriver driver remote pool = RemoteEndPool
  { freeRemoteEnds = MS.adjust (remote:) driver $ freeRemoteEnds pool
  }

-- | Attempt to pop a remote end from the pool stack for a given driver. Returns the new pool, whether or not a remote end was popped. Returns a `Just Just` if a remote end was found, a `Just Nothing` if the driver has an empty stack of remotes, and `Nothing` if the pool is undefined for the driver.
getRemoteEndForDriver :: DriverName -> RemoteEndPool -> (RemoteEndPool, Maybe (Maybe RemoteEnd))
getRemoteEndForDriver driver pool =
  case MS.lookup driver (freeRemoteEnds pool) of
    Nothing -> (pool, Nothing)
    Just z -> case z of
      [] -> (pool, Just Nothing)
      (r:rs) -> (pool { freeRemoteEnds = MS.insert driver rs $ freeRemoteEnds pool }, Just $ Just r)


-- | Representation of a remote end connection.
data RemoteEnd = RemoteEnd
  { remoteEndHost :: String -- ^ Scheme, auth, and hostname
  , remoteEndPort :: Int
  , remoteEndPath :: String -- ^ Additional path component
  } deriving (Eq, Show)


-- | Parse a remote end config file. This file consists of 0 or more blocks of the form
--
-- > DRIVER_NAME
-- > - REMOTE_END_URI
-- > - REMOTE_END_URI
--
-- where `DRIVER_NAME` is either `geckodriver` or `chromedriver` and each `REMOTE_END_URI` is the uri of a WebDriver remote end, including scheme. Blank lines are ignored.
parseRemoteEndConfig :: String -> Either String RemoteEndPool
parseRemoteEndConfig str = do
  freeEnds <- fmap (MS.fromListWith (++)) $ tokenizeRemoteEndConfig $ filter (/= "") $ lines str
  return RemoteEndPool
    { freeRemoteEnds = freeEnds
    }


tokenizeRemoteEndConfig :: [String] -> Either String [(DriverName, [RemoteEnd])]
tokenizeRemoteEndConfig ls = case ls of
  [] -> return []
  (first:rest) -> do
    driver <- case first of
      "geckodriver" -> return Geckodriver
      "chromedriver" -> return Chromedriver
      _ -> Left $ "Unrecognized driver name '" ++ first ++ "'."
    let (remotes, remainder) = span ("- " `isPrefixOf`) rest
    ends <- mapM (parseRemoteEnd . drop 2) remotes
    config <- tokenizeRemoteEndConfig remainder
    return $ (driver, nub ends) : config


-- | Parse a remote end command line option. This option consists of 0 or more substrings of the form
--
-- > DRIVER_NAME: REMOTE_END_URI REMOTE_END_URI ...
--
-- where `DRIVER_NAME` is either `geckodriver` or `chromedriver` and each `REMOTE_END_URI` is the uri of a WebDriver remote end, including scheme.
parseRemoteEndOption :: String -> Either String RemoteEndPool
parseRemoteEndOption str = do
  freeEnds <- fmap (MS.fromListWith (++)) $ tokenizeRemoteEndOption $ words str
  return RemoteEndPool
    { freeRemoteEnds = freeEnds
    }


tokenizeRemoteEndOption :: [String] -> Either String [(DriverName, [RemoteEnd])]
tokenizeRemoteEndOption ws = case ws of
  [] -> return []
  (first:rest) -> do
    driver <- case first of
      "geckodriver:" -> return Geckodriver
      "chromedriver:" -> return Chromedriver
      _ -> Left $ "Unrecognized driver name '" ++ first ++ "'."
    let (remotes, remainder) = break (isSuffixOf ":") rest
    ends <- mapM parseRemoteEnd remotes
    option <- tokenizeRemoteEndOption remainder
    return $ (driver, nub ends) : option


-- | Parse a single remote end URI. Must include the scheme (http:// or https://) even though this is redundant.
parseRemoteEnd :: String -> Either String RemoteEnd
parseRemoteEnd str = case parseURI str of
  Nothing -> Left $ "Could not parse remote end URI '" ++ str ++ "'."
  Just URI{..} -> case uriAuthority of
    Nothing -> Left $ "Error parsing authority for URI '" ++ str ++ "'."
    Just URIAuth{..} -> case uriPort of
      "" -> Right RemoteEnd
        { remoteEndHost = uriUserInfo ++ uriRegName
        , remoteEndPort = 4444
        , remoteEndPath = uriPath
        }
      ':':ds -> case readMaybe ds of
        Nothing -> Left $ "Error parsing port for URI '" ++ str ++ "'."
        Just k -> Right RemoteEnd
          { remoteEndHost = uriUserInfo ++ uriRegName
          , remoteEndPort = k
          , remoteEndPath = uriPath
          }
      p -> Left $ "Unexpected port '" ++ p ++ "' in URI '" ++ str ++ "'."

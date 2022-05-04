{- |
Module      : Test.Tasty.WebDriver.Config
Description : Helpers for parsing config files.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
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

  , parseOptionWithArgument
  ) where

import Data.List
  ( isPrefixOf, nub )
import qualified Data.Map.Strict as MS
  ( fromListWith, insert, lookup, adjust, fromList, unionWith, Map )
import Data.Typeable
  ( Typeable )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.URI
  ( URI(..), URIAuth(..), parseURI )
import Text.Read
  ( readMaybe )



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

instance Semigroup RemoteEndPool where
  x <> y = RemoteEndPool
    { freeRemoteEnds = MS.unionWith (++) (freeRemoteEnds x) (freeRemoteEnds y)
    }

instance Monoid RemoteEndPool where
  mempty = RemoteEndPool
    { freeRemoteEnds = MS.fromList []
    }

  mappend = (<>)

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
  { remoteEndHost :: Text -- ^ Scheme, auth, and hostname
  , remoteEndPort :: Int
  , remoteEndPath :: Text -- ^ Additional path component
  } deriving Eq

instance Show RemoteEnd where
  show remote = T.unpack $ T.concat
    [ remoteEndHost remote
    , ":"
    , T.pack $ show $ remoteEndPort remote
    , remoteEndPath remote
    ]

-- | Parse a remote end config file. This file consists of 0 or more blocks of the form
--
-- > DRIVER_NAME
-- > - REMOTE_END_URI
-- > - REMOTE_END_URI
--
-- where `DRIVER_NAME` is either `geckodriver` or `chromedriver` and each `REMOTE_END_URI` is the uri of a WebDriver remote end, including scheme. Blank lines are ignored.
parseRemoteEndConfig :: Text -> Either Text RemoteEndPool
parseRemoteEndConfig str = do
  freeEnds <- fmap (MS.fromListWith (<>)) $ tokenizeRemoteEndConfig $ filter (/= "") $ T.lines str
  return RemoteEndPool
    { freeRemoteEnds = freeEnds
    }

tokenizeRemoteEndConfig :: [Text] -> Either Text [(DriverName, [RemoteEnd])]
tokenizeRemoteEndConfig ls = case ls of
  [] -> return []
  (first:rest) -> do
    driver <- case first of
      "geckodriver" -> return Geckodriver
      "chromedriver" -> return Chromedriver
      _ -> Left $ "Unrecognized driver name '" <> first <> "'."
    let (remotes, remainder) = span ("- " `T.isPrefixOf`) rest
    ends <- mapM (parseRemoteEnd . T.drop 2) remotes
    config <- tokenizeRemoteEndConfig remainder
    return $ (driver, nub ends) : config

-- | Parse a remote end command line option. This option consists of 0 or more substrings of the form
--
-- > DRIVER_NAME: REMOTE_END_URI REMOTE_END_URI ...
--
-- where `DRIVER_NAME` is either `geckodriver` or `chromedriver` and each `REMOTE_END_URI` is the uri of a WebDriver remote end, including scheme.
parseRemoteEndOption :: Text -> Either Text RemoteEndPool
parseRemoteEndOption str = do
  freeEnds <- fmap (MS.fromListWith (<>)) $ tokenizeRemoteEndOption $ T.words str
  return RemoteEndPool
    { freeRemoteEnds = freeEnds
    }

tokenizeRemoteEndOption :: [Text] -> Either Text [(DriverName, [RemoteEnd])]
tokenizeRemoteEndOption ws = case ws of
  [] -> return []
  (first:rest) -> do
    driver <- case first of
      "geckodriver" -> return Geckodriver
      "chromedriver" -> return Chromedriver
      _ -> Left $ "Unrecognized driver name '" <> first <> "'."
    let (remotes, remainder) = break (`elem` ["geckodriver","chromedriver"]) rest
    ends <- mapM parseRemoteEnd remotes
    option <- tokenizeRemoteEndOption remainder
    return $ (driver, nub ends) : option

-- | Parse a single remote end URI. Must include the scheme (http:// or https://) even though this is redundant.
parseRemoteEnd :: Text -> Either Text RemoteEnd
parseRemoteEnd str = case parseURI $ T.unpack str of
  Nothing -> Left $ "Could not parse remote end URI '" <> str <> "'."
  Just URI{..} -> case uriAuthority of
    Nothing -> Left $ "Error parsing authority for URI '" <> str <> "'."
    Just URIAuth{..} -> case uriPort of
      "" -> Right RemoteEnd
        { remoteEndHost = T.pack $ uriUserInfo <> uriRegName
        , remoteEndPort = 4444
        , remoteEndPath = T.pack uriPath
        }
      ':' : ds -> case readMaybe ds of
        Nothing -> Left $ "Error parsing port for URI '" <> str <> "'."
        Just k -> Right RemoteEnd
          { remoteEndHost = T.pack $ uriUserInfo <> uriRegName
          , remoteEndPort = k
          , remoteEndPath = T.pack uriPath
          }
      _ -> Left $ "Unexpected port '" <> T.pack uriPort <> "' in URI '" <> str <> "'."


-- | Helper function for parsing command line options with a required argument. Assumes long-form option names starting with a hyphen. Note the return type; @Just Nothing@ indicates that the option was not present, while @Nothing@ indicates that the option was present but its required argument was not.
parseOptionWithArgument
  :: Text -- ^ Option to parse for, including hyphen(s).
  -> [Text] -- ^ List of command line arguments.
  -> Maybe (Maybe Text)
parseOptionWithArgument option args = case args of
  (opt:arg:rest) -> if opt == option
    then case T.uncons arg of
      Just (c,cs) -> if c == '-' then Nothing else Just (Just arg)
      Nothing -> Just (Just arg)
    else parseOptionWithArgument option (arg:rest)
  _ -> Just Nothing

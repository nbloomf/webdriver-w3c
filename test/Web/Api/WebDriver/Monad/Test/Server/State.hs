module Web.Api.WebDriver.Monad.Test.Server.State (
    WebDriverServerState()
  , defaultWebDriverServerState
  , _is_active_session
  , _create_session
  , _delete_session
  , _get_current_url
  , _set_current_url
  ) where

import Data.List (delete)

-- | Models the internal state of a WebDriver remote end.
data WebDriverServerState = WebDriverServerState
  { _next_session_id :: Int

  , _readiness_state :: Bool
  , _active_sessions :: [String]
  , _max_active_sessions :: Int

  , _current_url :: String
  } deriving Show

defaultWebDriverServerState :: WebDriverServerState
defaultWebDriverServerState = WebDriverServerState
  { _next_session_id = 1

  , _readiness_state = True
  , _active_sessions = []
  , _max_active_sessions = 1

  , _current_url = ""
  }

_is_active_session
  :: String
  -> WebDriverServerState
  -> Bool
_is_active_session str st =
  elem str (_active_sessions st)

_create_session
  :: WebDriverServerState
  -> Maybe (String, WebDriverServerState)
_create_session st =
  if True == _readiness_state st
       && length (_active_sessions st) < _max_active_sessions st
    then
      let
        _id = show $ _next_session_id st
        _st = st
          { _next_session_id =
              1 + (_next_session_id st)
          , _active_sessions =
              _id : _active_sessions st
          , _readiness_state = False
          }
      in Just (_id, _st)
    else Nothing

_delete_session
  :: String
  -> WebDriverServerState
  -> WebDriverServerState
_delete_session str st =
  st { _active_sessions = delete str $ _active_sessions st }

_get_current_url
  :: WebDriverServerState
  -> String
_get_current_url =
  _current_url

_set_current_url
  :: String
  -> WebDriverServerState
  -> WebDriverServerState
_set_current_url url st = st
  { _current_url = url
  }

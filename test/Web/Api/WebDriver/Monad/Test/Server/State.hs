module Web.Api.WebDriver.Monad.Test.Server.State (
    WebDriverServerState()
  , defaultWebDriverServerState
  , _is_active_session
  , _create_session
  , _delete_session
  , _load_page
  , _current_page
  , _go_back
  , _go_forward
  , _get_current_url
  , _get_last_selected_element
  , _set_last_selected_element
  ) where

import Data.List (delete)
import Web.Api.WebDriver.Monad.Test.Server.Page

-- | Models the internal state of a WebDriver remote end.
data WebDriverServerState = WebDriverServerState
  { _next_session_id :: Int

  , _readiness_state :: Bool
  , _active_sessions :: [String]
  , _max_active_sessions :: Int

  , _history :: [Page]
  , _future :: [Page]

  , _last_selected_element :: Maybe String

  , _current_page :: Page
  , _internets :: [Page]
  } deriving Show

defaultWebDriverServerState :: WebDriverServerState
defaultWebDriverServerState = WebDriverServerState
  { _next_session_id = 1

  , _readiness_state = True
  , _active_sessions = []
  , _max_active_sessions = 1

  , _history = []
  , _future = []

  , _last_selected_element = Nothing

  , _current_page = _default_page
  , _internets = []
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
_delete_session str st = st
  { _active_sessions = delete str $ _active_sessions st
  }

_get_current_url
  :: WebDriverServerState
  -> String
_get_current_url =
  url . _current_page

_load_page
  :: String
  -> WebDriverServerState
  -> Maybe WebDriverServerState
_load_page path st = do
  let file = fileOnly path
  p <- case file of
    "success.html"             -> return _success_page
    "example.com"              -> return _success_page
    "invalidElementState.html" -> return _invalidElementState_page
    "about:blank"              -> return pageAboutBlank
    _                          -> requestPage path (_internets st)
  return $ st
    { _current_page = p
    , _history = (_current_page st) : _history st
    }

_go_back
  :: WebDriverServerState
  -> WebDriverServerState
_go_back st = case _history st of
  [] -> st
  p:ps -> st
    { _current_page = p
    , _future = (_current_page st) : _future st
    , _history = ps
    }

_go_forward
  :: WebDriverServerState
  -> WebDriverServerState
_go_forward st = case _future st of
  [] -> st
  p:ps -> st
    { _current_page = p
    , _history = (_current_page st) : _history st
    , _future = ps
    }

_get_last_selected_element
  :: WebDriverServerState
  -> Maybe String
_get_last_selected_element =
  _last_selected_element

_set_last_selected_element
  :: String
  -> WebDriverServerState
  -> WebDriverServerState
_set_last_selected_element elt st = st
  { _last_selected_element = Just elt
  }

_default_page :: Page
_default_page = buildPage "localhost" $
  node Html []
    [ node Head []
        [ node Title []
            [ Text "localhost"
            ]
        ]
    , node Body []
        [
        ]
    ]

_success_page :: Page
_success_page = buildPage "success.html" $
  node Html []
    [ node Head []
        [ node Title []
            [ Text "successes"
            ]
        ]
    , node Body []
        [ node Form []
            [ node P [(Id, Just "super-cool")] []
            , node Button [(Id, Just "alert-button")] []
            , node Button [(Id, Just "confirm-button")] []
            , node Button [(Id, Just "prompt-button")] []
            , node Button [(Id, Just "add-cookie-button")] []
            , node Input [(Name, Just "sometext")] []
            ]
        , node Div [(Class, Just "test")] []
        ]
    ]

_invalidElementState_page :: Page
_invalidElementState_page = buildPage "invalidElementState.html" $
  node Html []
    [ node Head []
        [ node Title []
            [ Text "successes"
            ]
        ]
    , node Body [] []
    ]

fileOnly :: String -> String
fileOnly = reverse . takeWhile (/= '/') . reverse

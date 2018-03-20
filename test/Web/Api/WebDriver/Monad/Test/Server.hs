{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Web.Api.WebDriver.Monad.Test.Server (
    WebDriverServerState(..)
  , defaultWebDriverServerState
  , defaultWebDriverServer
  ) where

import Data.List
import Data.Text (Text, pack, unpack)
import qualified Data.HashMap.Strict as HMS
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Data.HashMap.Strict

import Web.Api.Http
import Web.Api.Http.Effects.Test.Mock
import Web.Api.Http.Effects.Test.Server
import Web.Api.WebDriver.Monad.Test.Server.State

defaultWebDriverServer :: MockServer WebDriverServerState
defaultWebDriverServer = MockServer
  { __http_get = \st !url -> case splitUrl $ stripScheme url of
      [_,"session",session_id,"url"] ->
        get_session_id_url st session_id

      [_,"session",session_id,"title"] ->
        get_session_id_title st session_id

      _ -> error $ "defaultWebDriverServer: get url: " ++ url

  , __http_post = \st !url !payload -> case splitUrl $ stripScheme url of
      [_,"session"] ->
        post_session st

      [_,"session",session_id,"url"] ->
        post_session_id_url st session_id payload

      [_,"session",session_id,"back"] ->
        post_session_id_back st session_id

      [_,"session",session_id,"forward"] ->
        post_session_id_forward st session_id

      [_,"session",session_id,"refresh"] ->
        post_session_id_refresh st session_id

      [_,"session",session_id,"window","maximize"] ->
        post_session_id_window_maximize st session_id

      [_,"session",session_id,"window","minimize"] ->
        post_session_id_window_minimize st session_id

      [_,"session",session_id,"window","fullscreen"] ->
        post_session_id_window_fullscreen st session_id

      _ -> error $ "defaultWebDriverServer: post url: " ++ stripScheme url

  , __http_delete = \st url -> case splitUrl $ stripScheme url of
       [_,"session",session_id] ->
         (_success_with_empty_object, st)

       _ -> error $ "defaultWebDriverServer: delete url: " ++ url
  }

stripScheme :: String -> String
stripScheme str = case str of
  ('h':'t':'t':'p':':':'/':'/':rest) -> rest
  ('h':'t':'t':'p':'s':':':'/':'/':rest) -> rest
  _ -> str

splitUrl :: String -> [String]
splitUrl = unfoldr foo
  where
    foo [] = Nothing
    foo xs = case span (/= '/') xs of
      (as,[]) -> Just (as,[])
      (as,b:bs) -> Just (as,bs)



{--------------------}
{- Request Handlers -}
{--------------------}

get_session_id_url
  :: WebDriverServerState
  -> String
  -> (HttpResponse, WebDriverServerState)
get_session_id_url st session_id =
  if not $ _is_active_session session_id st
    then (_err_invalid_session_id, st)
    else (_success_with_value $ String $ pack $ _get_current_url st, st)

get_session_id_title
  :: WebDriverServerState
  -> String
  -> (HttpResponse, WebDriverServerState)
get_session_id_title st session_id =
  if not $ _is_active_session session_id st
    then (_err_invalid_session_id, st)
    else (_success_with_value $ String $ pack "fake title", st)

post_session
  :: WebDriverServerState
  -> (HttpResponse, WebDriverServerState)
post_session st =
  let
    result = _create_session st
  in
    case result of
      Nothing -> (_err_session_not_created, st)
      Just (_id, _st) ->
        let
          response = _success_with_value $ object [ ("sessionId", String $ pack _id) ]
        in (response, _st)

post_session_id_url
  :: WebDriverServerState
  -> String
  -> LB.ByteString
  -> (HttpResponse, WebDriverServerState)
post_session_id_url !st !session_id !payload =
  if not $ _is_active_session session_id st
    then (_err_invalid_session_id, st)
    else case decode payload of
      Nothing -> (_err_invalid_argument, st)
      Just (Object m) -> case HMS.lookup "url" m of
        Nothing -> (_err_invalid_argument, st)
        Just (String url) ->
          let _st = _set_current_url (unpack url) st
          in (_success_with_empty_object, _st)
      Just _ -> (_err_invalid_argument, st)

post_session_id_back
  :: WebDriverServerState
  -> String
  -> (HttpResponse, WebDriverServerState)
post_session_id_back !st !session_id =
  let
    response = if _is_active_session session_id st
      then _success_with_empty_object
      else _err_invalid_session_id
  in (response, st)

post_session_id_forward
  :: WebDriverServerState
  -> String
  -> (HttpResponse, WebDriverServerState)
post_session_id_forward !st !session_id =
  let
    response = if _is_active_session session_id st
      then _success_with_empty_object
      else _err_invalid_session_id
  in (response, st)

post_session_id_refresh
  :: WebDriverServerState
  -> String
  -> (HttpResponse, WebDriverServerState)
post_session_id_refresh !st !session_id =
  let
    response = if _is_active_session session_id st
      then _success_with_empty_object
      else _err_invalid_session_id
  in (response, st)

post_session_id_window_maximize
  :: WebDriverServerState
  -> String
  -> (HttpResponse, WebDriverServerState)
post_session_id_window_maximize !st !session_id =
  let
    response = if _is_active_session session_id st
      then _success_with_value $ object
        [ ("x", Number 0)
        , ("y", Number 0)
        , ("height", Number 480)
        , ("width", Number 640)
        ]
      else _err_invalid_session_id
  in (response, st)

post_session_id_window_minimize
  :: WebDriverServerState
  -> String
  -> (HttpResponse, WebDriverServerState)
post_session_id_window_minimize !st !session_id =
  let
    response = if _is_active_session session_id st
      then _success_with_value $ object
        [ ("x", Number 0)
        , ("y", Number 0)
        , ("height", Number 0)
        , ("width", Number 0)
        ]
      else _err_invalid_session_id
  in (response, st)

post_session_id_window_fullscreen
  :: WebDriverServerState
  -> String
  -> (HttpResponse, WebDriverServerState)
post_session_id_window_fullscreen !st !session_id =
  let
    response = if _is_active_session session_id st
      then _success_with_value $ object
        [ ("x", Number 0)
        , ("y", Number 0)
        , ("height", Number 480)
        , ("width", Number 640)
        ]
      else _err_invalid_session_id
  in (response, st)



{---------------------}
{- Success Responses -}
{---------------------}

_success_with_empty_object :: HttpResponse
_success_with_empty_object = _success_with_value $ object []

_success_with_value :: Value -> HttpResponse
_success_with_value v =
  _200_Ok $ encode $ object [ ("value", v ) ]



{-------------------}
{- Error Responses -}
{-------------------}

-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#handling-errors>.
errorObject :: String -> String -> String -> Maybe String -> LB.ByteString
errorObject err msg stk dat = case dat of
  Nothing -> encode $ object [ ("value", object
    [ ("error", String $ pack err)
    , ("message", String $ pack msg)
    , ("stacktrace", String $ pack stk)
    ] ) ]
  Just txt -> encode $ object [ ("value", object
    [ ("error", String $ pack err)
    , ("message", String $ pack msg)
    , ("stacktrace", String $ pack stk)
    , ("data", object
      [ ("text", String $ pack txt)
      ] )
    ] ) ]

_err_element_click_intercepted :: HttpResponse
_err_element_click_intercepted =
  _400_Bad_Request $ errorObject
    "element click intercepted" "" "" Nothing

_err_element_not_selectable :: HttpResponse
_err_element_not_selectable =
  _400_Bad_Request $ errorObject
    "element not selectable" "" "" Nothing

_err_invalid_argument :: HttpResponse
_err_invalid_argument =
  _400_Bad_Request $ errorObject
    "invalid argument" "" "" Nothing

_err_invalid_session_id :: HttpResponse
_err_invalid_session_id =
  _404_Not_Found $ errorObject
    "invalid session id" "" "" Nothing

_err_session_not_created :: HttpResponse
_err_session_not_created =
  _500_Internal_Server_Error $ errorObject
    "session_not_created" "" "" Nothing

_err_no_such_alert :: HttpResponse
_err_no_such_alert =
  _404_Not_Found $ errorObject
    "no such alert" "" "" Nothing

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
import qualified Data.ByteString as SB
import Data.Aeson
import Data.HashMap.Strict
import Network.HTTP.Client (HttpException)
import Network.HTTP.Client.Internal
import Network.HTTP.Types

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

      [_,"session",session_id,"timeouts"] ->
        get_session_id_timeouts st session_id

      [_,"session",session_id,"window","rect"] ->
        get_session_id_window_rect st session_id

      [_,"session",session_id,"window"] ->
        get_session_id_window st session_id

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

      [_,"session",session_id,"element"] ->
        post_session_id_element st session_id payload

      [_,"session",session_id,"elements"] ->
        post_session_id_elements st session_id payload

      [_,"session",session_id,"element",element_id,"element"] ->
        post_session_id_element_id_element st session_id element_id payload

      [_,"session",session_id,"element",element_id,"elements"] ->
        post_session_id_element_id_elements st session_id element_id payload

      [_,"session",session_id,"actions"] ->
        post_session_id_actions st session_id payload

      _ -> error $ "defaultWebDriverServer: post url: " ++ stripScheme url

  , __http_delete = \st !url -> case splitUrl $ stripScheme url of
      [_,"session",session_id] ->
        (Right _success_with_empty_object, st)

      [_,"session",session_id,"cookie"] ->
        delete_session_id_cookie st session_id

      [_,"session",session_id,"window"] ->
        delete_session_id_window st session_id

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
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_url st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ String $ pack $ _get_current_url st, st)

get_session_id_title
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_title st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ String $ pack "fake title", st)

get_session_id_timeouts
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_timeouts st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ object [("script", Number 0),("pageLoad", Number 0),("implicit", Number 0)], st)

get_session_id_window_rect
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_window_rect st session_id =
  if _is_active_session session_id st
      then (Right $ _success_with_value $ object
        [ ("x", Number 0)
        , ("y", Number 0)
        , ("height", Number 480)
        , ("width", Number 640)
        ], st)
      else (Left _err_invalid_session_id, st)

post_session
  :: WebDriverServerState
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session st =
  let
    result = _create_session st
  in
    case result of
      Nothing -> (Left _err_session_not_created, st)
      Just (_id, _st) ->
        let
          response = _success_with_value $ object [ ("sessionId", String $ pack _id) ]
        in (Right response, _st)

post_session_id_url
  :: WebDriverServerState
  -> String
  -> LB.ByteString
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_url !st !session_id !payload =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else case decode payload of
      Nothing -> (Left _err_invalid_argument, st)
      Just (Object m) -> case HMS.lookup "url" m of
        Nothing -> (Left _err_invalid_argument, st)
        Just (String url) -> case url of
          "https://fake.example" -> (Left _err_unknown_error, st)
          _ -> 
            let _st = _set_current_url (unpack url) st
            in (Right _success_with_empty_object, _st)
      Just _ -> (Left _err_invalid_argument, st)

post_session_id_back
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_back !st !session_id =
  if _is_active_session session_id st
      then (Right _success_with_empty_object, st)
      else (Left _err_invalid_session_id, st)

post_session_id_forward
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_forward !st !session_id =
  if _is_active_session session_id st
      then (Right _success_with_empty_object, st)
      else (Left _err_invalid_session_id, st)

post_session_id_refresh
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_refresh !st !session_id =
  if _is_active_session session_id st
      then (Right _success_with_empty_object, st)
      else (Left _err_invalid_session_id, st)

post_session_id_window_maximize
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_window_maximize !st !session_id =
  if _is_active_session session_id st
      then (Right $ _success_with_value $ object
        [ ("x", Number 0)
        , ("y", Number 0)
        , ("height", Number 480)
        , ("width", Number 640)
        ], st)
      else (Left _err_invalid_session_id, st)

post_session_id_window_minimize
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_window_minimize !st !session_id =
  if _is_active_session session_id st
      then (Right $ _success_with_value $ object
        [ ("x", Number 0)
        , ("y", Number 0)
        , ("height", Number 0)
        , ("width", Number 0)
        ], st)
      else (Left _err_invalid_session_id, st)

post_session_id_window_fullscreen
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_window_fullscreen !st !session_id =
  if _is_active_session session_id st
      then (Right $ _success_with_value $ object
        [ ("x", Number 0)
        , ("y", Number 0)
        , ("height", Number 480)
        , ("width", Number 640)
        ], st)
      else (Left _err_invalid_session_id, st)

post_session_id_element
  :: WebDriverServerState
  -> String
  -> LB.ByteString
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_element st session_id payload =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ object
      [ ("element-6066-11e4-a52e-4f735466cecf", String "element-id")
      ], st)

post_session_id_elements
  :: WebDriverServerState
  -> String
  -> LB.ByteString
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_elements st session_id payload =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ toJSONList [object
      [ ("element-6066-11e4-a52e-4f735466cecf", String "element-id")
      ]], st)

post_session_id_element_id_element
  :: WebDriverServerState
  -> String
  -> String
  -> LB.ByteString
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_element_id_element st session_id element_id payload =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ object
      [ ("element-6066-11e4-a52e-4f735466cecf", String "element-id")
      ], st)

post_session_id_element_id_elements
  :: WebDriverServerState
  -> String
  -> String
  -> LB.ByteString
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_element_id_elements st session_id element_id payload =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ toJSONList [object
      [ ("element-6066-11e4-a52e-4f735466cecf", String "element-id")
      ]], st)

post_session_id_actions
  :: WebDriverServerState
  -> String
  -> LB.ByteString
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_actions !st !session_id payload =
  if _is_active_session session_id st
    then (Right _success_with_empty_object, st)
    else (Left _err_invalid_session_id, st)

get_session_id_window
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_window st session_id =
  if _is_active_session session_id st
    then (Right $ _success_with_value $ String "window-1", st)
    else (Left _err_invalid_session_id, st)

delete_session_id_cookie
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
delete_session_id_cookie st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right _success_with_empty_object, st)

delete_session_id_window
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
delete_session_id_window st session_id =
  if _is_active_session session_id st
    then (Right $ _success_with_value $ toJSONList [String "window-1"], st)
    else (Left _err_invalid_session_id, st)



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
errorObject :: String -> String -> String -> Maybe String -> SB.ByteString
errorObject err msg stk dat = case dat of
  Nothing -> LB.toStrict $ encode $ object [ ("value", object
    [ ("error", String $ pack err)
    , ("message", String $ pack msg)
    , ("stacktrace", String $ pack stk)
    ] ) ]
  Just txt -> LB.toStrict $ encode $ object [ ("value", object
    [ ("error", String $ pack err)
    , ("message", String $ pack msg)
    , ("stacktrace", String $ pack stk)
    , ("data", object
      [ ("text", String $ pack txt)
      ] )
    ] ) ]

emptyResponse :: Response ()
emptyResponse = Response
  { responseStatus = ok200
  , responseVersion = http11
  , responseHeaders = []
  , responseBody = ()
  , responseCookieJar = CJ []
  , responseClose' = ResponseClose $ return ()
  }

_err_invalid_argument :: HttpException
_err_invalid_argument =
  HttpExceptionRequest undefined $ StatusCodeException
    (emptyResponse { responseStatus = badRequest400 })
    (errorObject "invalid argument" "" "" Nothing)

_err_invalid_session_id :: HttpException
_err_invalid_session_id =
  HttpExceptionRequest undefined $ StatusCodeException
    (emptyResponse { responseStatus = notFound404 })
    (errorObject "invalid session id" "" "" Nothing)

_err_session_not_created :: HttpException
_err_session_not_created =
  HttpExceptionRequest undefined $ StatusCodeException
    (emptyResponse { responseStatus = internalServerError500 })
    (errorObject "session not created" "" "" Nothing)

_err_unknown_error :: HttpException
_err_unknown_error =
  HttpExceptionRequest undefined $ StatusCodeException
    (emptyResponse { responseStatus = internalServerError500 })
    (errorObject "unknown error" "" "" Nothing)

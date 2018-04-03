{-# LANGUAGE OverloadedStrings, BangPatterns, FlexibleInstances #-}
module Web.Api.WebDriver.Monad.Test.Server (
    WebDriverServerState(..)
  , defaultWebDriverServerState
  , defaultWebDriverServer
  ) where

import Data.List
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.HashMap.Strict as HMS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import qualified Data.ByteString.Base64 as B64
import Data.Aeson
import Data.HashMap.Strict
import Network.HTTP.Client (HttpException)
import Network.HTTP.Client.Internal
import Network.HTTP.Types
import qualified Network.Wreq.Session as WreqS
import Codec.Picture
import Codec.Picture.Saving

import Web.Api.Http
import Web.Api.Http.Effects.Test.Mock
import Web.Api.Http.Effects.Test.Server
import Web.Api.WebDriver.Monad.Test.Server.State

instance Effectful (MockIO WebDriverServerState) where
  toIO x = do
    session <- WreqS.newSession
    let
      init = mockSt defaultWebDriverServer
        session defaultWebDriverServerState

      (a,_) = runMockIO x init
    return a

defaultWebDriverServer :: MockServer WebDriverServerState
defaultWebDriverServer = MockServer
  { __http_get = \st !url -> case splitUrl $ stripScheme url of
      [_,"status"] ->
        get_session_id_status st

      [_,"session",session_id,"timeouts"] ->
        get_session_id_timeouts st session_id

      [_,"session",session_id,"url"] ->
        get_session_id_url st session_id

      [_,"session",session_id,"title"] ->
        get_session_id_title st session_id

      [_,"session",session_id,"window"] ->
        get_session_id_window st session_id

      [_,"session",session_id,"window","handles"] ->
        get_session_id_window_handles st session_id

      [_,"session",session_id,"window","rect"] ->
        get_session_id_window_rect st session_id

      [_,"session",session_id,"element","active"] ->
        get_session_id_element_active st session_id

      [_,"session",session_id,"element",element_id,"selected"] ->
        get_session_id_element_id_selected st session_id element_id

      [_,"session",session_id,"element",element_id,"attribute",name] ->
        get_session_id_element_id_attribute_name st session_id element_id name

      [_,"session",session_id,"element",element_id,"property",name] ->
        undefined

      [_,"session",session_id,"element",element_id,"css",name] ->
        undefined

      [_,"session",session_id,"element",element_id,"text"] ->
        get_session_id_element_id_text st session_id element_id

      [_,"session",session_id,"element",element_id,"name"] ->
        undefined

      [_,"session",session_id,"element",element_id,"rect"] ->
        get_session_id_element_id_rect st session_id element_id

      [_,"session",session_id,"element",element_id,"enabled"] ->
        get_session_id_element_id_enabled st session_id element_id

      [_,"session",session_id,"source"] ->
        get_session_id_source st session_id

      [_,"session",session_id,"cookie"] ->
        undefined

      [_,"session",session_id,"cookie",name] ->
        undefined

      [_,"session",session_id,"alert","text"] ->
        undefined

      [_,"session",session_id,"screenshot"] ->
        get_session_id_screenshot st session_id

      [_,"session",session_id,"element",element_id,"screenshot"] ->
        get_session_id_element_id_screenshot st session_id element_id

      _ -> error $ "defaultWebDriverServer: get url: " ++ url

  , __http_post = \st !url !payload -> case splitUrl $ stripScheme url of
      [_,"session"] ->
        post_session st

      [_,"session",session_id,"timeouts"] ->
        post_session_id_timeouts st session_id payload

      [_,"session",session_id,"url"] ->
        post_session_id_url st session_id payload

      [_,"session",session_id,"back"] ->
        post_session_id_back st session_id

      [_,"session",session_id,"forward"] ->
        post_session_id_forward st session_id

      [_,"session",session_id,"refresh"] ->
        post_session_id_refresh st session_id

      [_,"session",session_id,"window"] ->
        post_session_id_window st session_id payload

      [_,"session",session_id,"frame"] ->
        post_session_id_frame st session_id payload

      [_,"session",session_id,"frame","parent"] ->
        undefined

      [_,"session",session_id,"window","rect"] ->
        undefined

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

      [_,"session",session_id,"element",element_id,"click"] ->
        post_session_id_element_id_click st session_id element_id

      [_,"session",session_id,"element",element_id,"clear"] ->
        post_session_id_element_id_clear st session_id element_id

      [_,"session",session_id,"element",element_id,"value"] ->
        post_session_id_element_id_value st session_id element_id payload
 
      [_,"session",session_id,"execute","sync"] ->
        undefined

      [_,"session",session_id,"execute","async"] ->
        undefined

      [_,"session",session_id,"cookie"] ->
        undefined

      [_,"session",session_id,"actions"] ->
        post_session_id_actions st session_id payload

      [_,"session",session_id,"alert","dismiss"] ->
        undefined

      [_,"session",session_id,"alert","accept"] ->
        undefined

      [_,"session",session_id,"alert","text"] ->
        undefined

      _ -> error $ "defaultWebDriverServer: post url: " ++ stripScheme url

  , __http_delete = \st !url -> case splitUrl $ stripScheme url of
      [_,"session",session_id] ->
        delete_session_id st session_id

      [_,"session",session_id,"window"] ->
        delete_session_id_window st session_id

      [_,"session",session_id,"cookie",name] ->
        undefined

      [_,"session",session_id,"cookie"] ->
        delete_session_id_cookie st session_id

      [_,"session",session_id,"actions"] ->
        undefined

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

post_session
  :: WebDriverServerState
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session st =
  case _create_session st of
    Nothing -> (Left _err_session_not_created, st)
    Just (_id, _st) -> (Right $ _success_with_value $ object
      [ ("sessionId", String $ pack _id)
      ], _st)

delete_session_id
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
delete_session_id st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right _success_with_empty_object, _delete_session session_id st)

get_session_id_status
  :: WebDriverServerState
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_status st =
  (Right $ _success_with_value $ object
    [ ("ready", Bool True)
    , ("message", String "ready")
    ], st)

get_session_id_timeouts
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_timeouts st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ object
      [ ("script", Number 0)
      , ("pageLoad", Number 0)
      , ("implicit", Number 0)
      ], st)

post_session_id_timeouts
  :: WebDriverServerState
  -> String
  -> LB.ByteString
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_timeouts st session_id !payload =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else case decode payload of
      Nothing -> (Left _err_invalid_argument, st)
      Just (Object m) -> (Right _success_with_empty_object, st)
      Just _ -> (Left _err_invalid_argument, st)

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

get_session_id_url
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_url st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ String $ pack $ _get_current_url st, st)

post_session_id_back
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_back !st !session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right _success_with_empty_object, st)

post_session_id_forward
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_forward !st !session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right _success_with_empty_object, st)

post_session_id_refresh
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_refresh !st !session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right _success_with_empty_object, st)

get_session_id_title
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_title st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ String $ pack "fake title", st)

get_session_id_window
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_window st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ String "window-1", st)

delete_session_id_window
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
delete_session_id_window st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ toJSONList [String "window-1"], st)

post_session_id_window
  :: WebDriverServerState
  -> String
  -> LB.ByteString
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_window st session_id payload =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_empty_object, st)

get_session_id_window_handles
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_window_handles st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ toJSONList [ String "handle-id" ], st)

post_session_id_frame
  :: WebDriverServerState
  -> String
  -> LB.ByteString
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_frame st session_id !payload =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right _success_with_empty_object, st)

{- TODO: post_session_id_frame_parent -}

get_session_id_window_rect
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_window_rect st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ object
      [ ("x", Number 0)
      , ("y", Number 0)
      , ("height", Number 480)
      , ("width", Number 640)
      ], st)

{- TODO:: post_session_id_window_rect -}

post_session_id_window_maximize
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_window_maximize !st !session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ object
      [ ("x", Number 0)
      , ("y", Number 0)
      , ("height", Number 480)
      , ("width", Number 640)
      ], st)

post_session_id_window_minimize
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_window_minimize !st !session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ object
      [ ("x", Number 0)
      , ("y", Number 0)
      , ("height", Number 0)
      , ("width", Number 0)
      ], st)

post_session_id_window_fullscreen
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_window_fullscreen !st !session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ object
      [ ("x", Number 0)
      , ("y", Number 0)
      , ("height", Number 480)
      , ("width", Number 640)
      ], st)

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

get_session_id_element_active
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_element_active st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ object
      [ ("element-6066-11e4-a52e-4f735466cecf", String "element-id")
      ], st)

get_session_id_element_id_selected
  :: WebDriverServerState
  -> String
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_element_id_selected st session_id element_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ Bool True, st)

get_session_id_element_id_attribute_name
  :: WebDriverServerState
  -> String
  -> String
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_element_id_attribute_name st session_id element_id name =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ String "foo", st)

{- TODO: get_session_id_element_id_property_name -}

{- TODO: get_session_id_element_id_css_name -}

get_session_id_element_id_text
  :: WebDriverServerState
  -> String
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_element_id_text st session_id element_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ String "foo", st)

{- TODO: get_session_id_element_id_name -}

get_session_id_element_id_rect
  :: WebDriverServerState
  -> String
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_element_id_rect !st !session_id !element_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ object
      [ ("x", Number 0)
      , ("y", Number 0)
      , ("height", Number 48)
      , ("width", Number 64)
      ], st)

get_session_id_element_id_enabled
  :: WebDriverServerState
  -> String
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_element_id_enabled st session_id element_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ Bool True, st)

post_session_id_element_id_click
  :: WebDriverServerState
  -> String
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_element_id_click st session_id element_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right _success_with_empty_object, st)

post_session_id_element_id_clear
  :: WebDriverServerState
  -> String
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_element_id_clear st session_id element_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right _success_with_empty_object, st)

post_session_id_element_id_value
  :: WebDriverServerState
  -> String
  -> String
  -> LB.ByteString
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_element_id_value st session_id element_id payload =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right _success_with_empty_object, st)

get_session_id_source
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_source st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right $ _success_with_value $ String "the source", st)

{- TODO: post_session_id_execute_sync -}

{- TODO: post_session_id_execute_async -}

{- TODO: get_session_id_cookie -}

{- TODO: get_session_id_cookie_name -}

{- TODO: post_session_id_cookie -}

{- TODO: delete_session_id_cookie_name -}

delete_session_id_cookie
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
delete_session_id_cookie st session_id =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right _success_with_empty_object, st)

post_session_id_actions
  :: WebDriverServerState
  -> String
  -> LB.ByteString
  -> (Either HttpException HttpResponse, WebDriverServerState)
post_session_id_actions !st !session_id payload =
  if not $ _is_active_session session_id st
    then (Left _err_invalid_session_id, st)
    else (Right _success_with_empty_object, st)

{- TODO: delete_session_id_actions -}

{- TODO: post_session_id_alert_dismiss -}

{- TODO: post_session_id_alert_accept -}

{- TODO: get_session_id_alert_text -}

{- TODO: post_session_id_alert_text -}

get_session_id_screenshot
  :: WebDriverServerState
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_screenshot !st !session_id =
  let
    black :: Int -> Int -> PixelRGB8
    black _ _ = PixelRGB8 0 0 0

    img :: DynamicImage
    img = ImageRGB8 (generateImage black 640 480)

    resp :: Text
    resp = decodeUtf8 $ B64.encode $ LB.toStrict $ imageToPng img
  in
    if not $ _is_active_session session_id st
      then (Left _err_invalid_session_id, st)
      else (Right $ _success_with_value $ String resp, st)

get_session_id_element_id_screenshot
  :: WebDriverServerState
  -> String
  -> String
  -> (Either HttpException HttpResponse, WebDriverServerState)
get_session_id_element_id_screenshot st session_id element_id =
  let
    black :: Int -> Int -> PixelRGB8
    black _ _ = PixelRGB8 0 0 0

    img :: DynamicImage
    img = ImageRGB8 (generateImage black 64 48)

    resp :: Text
    resp = decodeUtf8 $ B64.encode $ LB.toStrict $ imageToPng img
  in
    if not $ _is_active_session session_id st
      then (Left _err_invalid_session_id, st)
      else (Right $ _success_with_value $ String resp, st)



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

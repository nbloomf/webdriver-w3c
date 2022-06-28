{-# LANGUAGE OverloadedStrings, BangPatterns, FlexibleInstances, RecordWildCards, CPP #-}
module Web.Api.WebDriver.Monad.Test.Server (
    WebDriverServerState(..)
  , defaultWebDriverServerState
  , defaultWebDriverServer
  ) where

import Data.List
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.HashMap.Strict as HMS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as SB
import qualified Data.ByteString.Base64 as B64
import qualified Data.Vector as V
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Data.HashMap.Strict
import Network.HTTP.Client (HttpException)
import Network.HTTP.Client.Internal
import Network.HTTP.Types
import qualified Network.Wreq.Session as WreqS
import Codec.Picture
import Codec.Picture.Saving

import Data.MockIO
import Data.MockIO.FileSystem

import Web.Api.WebDriver
import Web.Api.WebDriver.Monad.Test.Server.State
import Web.Api.WebDriver.Monad.Test.Server.Page



defaultWebDriverServer :: MockWorld WebDriverServerState
defaultWebDriverServer = MockWorld
  { _files = emptyFileSystem
  , _time = epoch
  , _serverState = MockServer defaultWebDriverServerState
  , _httpGet = \url -> case splitUrl $ stripScheme $ T.unpack url of
      {- Status -}
      [_,"status"] ->
        get_session_id_status

      {- Get Timeouts -}
      [_,"session",session_id,"timeouts"] ->
        get_session_id_timeouts session_id

      {- Get Current Url -}
      [_,"session",session_id,"url"] ->
        get_session_id_url session_id

      {- Get Title -}
      [_,"session",session_id,"title"] ->
        get_session_id_title session_id

      {- Get Window Handle -}
      [_,"session",session_id,"window"] ->
        get_session_id_window session_id

      {- Get Window Handles -}
      [_,"session",session_id,"window","handles"] ->
        get_session_id_window_handles session_id

      {- Get Window Rect -}
      [_,"session",session_id,"window","rect"] ->
        get_session_id_window_rect session_id

      {- Get Active Element -}
      [_,"session",session_id,"element","active"] ->
        get_session_id_element_active session_id

      {- Is Element Selected -}
      [_,"session",session_id,"element",element_id,"selected"] ->
        get_session_id_element_id_selected session_id element_id

      {- Get Element Attribute -}
      [_,"session",session_id,"element",element_id,"attribute",name] ->
        get_session_id_element_id_attribute_name session_id element_id name

      {- Get Element Property -}
      [_,"session",session_id,"element",element_id,"property",name] ->
        undefined

      {- Get Element CSS Value -}
      [_,"session",session_id,"element",element_id,"css",name] ->
        get_session_id_element_id_css_name session_id element_id name

      {- Get Element Text -}
      [_,"session",session_id,"element",element_id,"text"] ->
        get_session_id_element_id_text session_id element_id

      {- Get Element Tag Name -}
      [_,"session",session_id,"element",element_id,"name"] ->
        get_session_id_element_id_name session_id element_id

      {- Get Element Rect -}
      [_,"session",session_id,"element",element_id,"rect"] ->
        get_session_id_element_id_rect session_id element_id

      {- Is Element Enabled -}
      [_,"session",session_id,"element",element_id,"enabled"] ->
        get_session_id_element_id_enabled session_id element_id

      {- Get Computed Role -}
      [_,"session",session_id,"element",element_id,"computedrole"] ->
        get_session_id_element_id_computedrole session_id element_id

      {- Get Computed Label -}
      [_,"session",session_id,"element",element_id,"computedlabel"] ->
        get_session_id_element_id_computedlabel session_id element_id

      {- Get Page Source -}
      [_,"session",session_id,"source"] ->
        get_session_id_source session_id

      {- Get All Cookies -}
      [_,"session",session_id,"cookie"] ->
        get_session_id_cookie session_id

      {- Get Named Cookie -}
      [_,"session",session_id,"cookie",name] ->
        get_session_id_cookie_name session_id

      {- Get Alert Text -}
      [_,"session",session_id,"alert","text"] ->
        get_session_id_alert_text session_id

      {- Take Screenshot -}
      [_,"session",session_id,"screenshot"] ->
        get_session_id_screenshot session_id

      {- Take Element Screenshot -}
      [_,"session",session_id,"element",element_id,"screenshot"] ->
        get_session_id_element_id_screenshot session_id element_id

      _ -> error $ "defaultWebDriverServer: get url: " ++ T.unpack url ++
        "' parsed as " ++ (show $ splitUrl $ stripScheme $ T.unpack url)

  , _httpPost = \url payload -> case splitUrl $ stripScheme $ T.unpack url of
      {- New Session -}
      [_,"session"] ->
        post_session

      {- Set Timeouts -}
      [_,"session",session_id,"timeouts"] ->
        post_session_id_timeouts session_id payload

      {- Navigate To -}
      [_,"session",session_id,"url"] ->
        post_session_id_url session_id payload

      {- Back -}
      [_,"session",session_id,"back"] ->
        post_session_id_back session_id

      {- Forward -}
      [_,"session",session_id,"forward"] ->
        post_session_id_forward session_id

      {- Refresh -}
      [_,"session",session_id,"refresh"] ->
        post_session_id_refresh session_id

      {- Switch To Window -}
      [_,"session",session_id,"window"] ->
        post_session_id_window session_id payload

      {- New Window -}
      [_,"session",session_id,"window","new"] ->
        post_session_id_window_new session_id payload

      {- Switch To Frame -}
      [_,"session",session_id,"frame"] ->
        post_session_id_frame session_id payload

      {- Switch To Parent Frame -}
      [_,"session",session_id,"frame","parent"] ->
        post_session_id_frame_parent session_id

      {- Set Window Rect -}
      [_,"session",session_id,"window","rect"] ->
        post_session_id_window_rect session_id payload

      {- Maximize Window -}
      [_,"session",session_id,"window","maximize"] ->
        post_session_id_window_maximize session_id

      {- Minimize Window -}
      [_,"session",session_id,"window","minimize"] ->
        post_session_id_window_minimize session_id

      {- Fullscreen Window -}
      [_,"session",session_id,"window","fullscreen"] ->
        post_session_id_window_fullscreen session_id

      {- Find Element -}
      [_,"session",session_id,"element"] ->
        post_session_id_element session_id payload

      {- Find Elements -}
      [_,"session",session_id,"elements"] ->
        post_session_id_elements session_id payload

      {- Find Element From Element -}
      [_,"session",session_id,"element",element_id,"element"] ->
        post_session_id_element_id_element session_id element_id payload

      {- Find Elements From Element -}
      [_,"session",session_id,"element",element_id,"elements"] ->
        post_session_id_element_id_elements session_id element_id payload

      {- Element Click -}
      [_,"session",session_id,"element",element_id,"click"] ->
        post_session_id_element_id_click session_id element_id

      {- Element Clear -}
      [_,"session",session_id,"element",element_id,"clear"] ->
        post_session_id_element_id_clear session_id element_id

      {- Element Send Keys -}
      [_,"session",session_id,"element",element_id,"value"] ->
        post_session_id_element_id_value session_id element_id payload
 
      {- Execute Script -}
      [_,"session",session_id,"execute","sync"] ->
        undefined

      {- Execute Async Script -}
      [_,"session",session_id,"execute","async"] ->
        undefined

      {- Add Cookie -}
      [_,"session",session_id,"cookie"] ->
        post_session_id_cookie session_id payload

      {- Perform Actions -}
      [_,"session",session_id,"actions"] ->
        post_session_id_actions session_id payload

      {- Dismiss Alert -}
      [_,"session",session_id,"alert","dismiss"] ->
        post_session_id_alert_dismiss session_id

      {- Accept Alert -}
      [_,"session",session_id,"alert","accept"] ->
        post_session_id_alert_accept session_id

      {- Send Alert Text -}
      [_,"session",session_id,"alert","text"] ->
        post_session_id_alert_text session_id

      {- Print Page -}
      [_,"session",session_id,"print"] ->
        post_session_id_print session_id

      _ -> error $ "defaultWebDriverServer: post url: '" ++ T.unpack url ++
        "' parsed as " ++ (show $ splitUrl $ stripScheme $ T.unpack url)

  , _httpDelete = \url -> case splitUrl $ stripScheme $ T.unpack url of
      {- Delete Session -}
      [_,"session",session_id] ->
        delete_session_id session_id

      {- Close Window -}
      [_,"session",session_id,"window"] ->
        delete_session_id_window session_id

      {- Delete Cookie -}
      [_,"session",session_id,"cookie",name] ->
        delete_session_id_cookie_name session_id name

      {- Delete All Cookies -}
      [_,"session",session_id,"cookie"] ->
        delete_session_id_cookie session_id

      {- Release Actions -}
      [_,"session",session_id,"actions"] ->
        delete_session_id_actions session_id

      _ -> error $ "defaultWebDriverServer: delete url: " ++ T.unpack url ++
        "' parsed as " ++ (show $ splitUrl $ stripScheme $ T.unpack url)
  }

stripScheme :: String -> String
stripScheme str = case dropWhile (== ' ') str of
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

{- New Session -}

post_session
  :: MockNetwork WebDriverServerState HttpResponse
post_session = do
  st <- getMockServer
  case _create_session st of
    Nothing -> errorMockNetwork _err_session_not_created
    Just (_id, _st) -> do
      modifyMockServer (const _st)
      return $ _success_with_value $ object
        [ ("sessionId", String $ pack _id)
        ]


{- Delete Session -}

delete_session_id
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
delete_session_id session_id = do
  verifyIsActiveSession session_id
  closeTheSession session_id
  return _success_with_empty_object


{- Status -}

get_session_id_status
  :: MockNetwork WebDriverServerState HttpResponse
get_session_id_status = do
  return $ _success_with_value $ object
    [ ("ready", Bool True)
    , ("message", String "ready")
    ]


{- Get Timeouts -}

get_session_id_timeouts
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_timeouts session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ object
    [ ("script", Number 0)
    , ("pageLoad", Number 0)
    , ("implicit", Number 0)
    ]


{- Set Timeouts -}

post_session_id_timeouts
  :: String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_timeouts session_id !payload = do
  verifyIsActiveSession session_id
  case decode payload of
    Nothing -> errorMockNetwork (_err_invalid_argument "Timeouts did not parse")
    Just (Object m) -> return _success_with_empty_object
    Just _ -> errorMockNetwork (_err_invalid_argument "Timeouts must be an object")


{- Navigate To -}

post_session_id_url
  :: String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_url !session_id !payload = do
  verifyIsActiveSession session_id
  url <- getAProperty "url" payload
  st <- getMockServer
  case _load_page url st of
    Nothing -> errorMockNetwork _err_unknown_error
    Just _st -> do
      modifyMockServer (const _st)
      return _success_with_empty_object


{- Get Current Url -}

get_session_id_url
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_url session_id = do
  verifyIsActiveSession session_id
  url <- fmap _get_current_url $ getMockServer
  return $ _success_with_value $ String $ pack url


{- Back -}

post_session_id_back
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_back !session_id = do
  verifyIsActiveSession session_id
  modifyMockServer _go_back
  return _success_with_empty_object


{- Forward -}

post_session_id_forward
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_forward !session_id = do
  verifyIsActiveSession session_id
  modifyMockServer _go_forward
  return _success_with_empty_object


{- Refresh -}

post_session_id_refresh
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_refresh !session_id = do
  verifyIsActiveSession session_id
  return _success_with_empty_object


{- Get Title -}

get_session_id_title
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_title session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ String $ pack "fake title"


{- Get Window Handle -}

get_session_id_window
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_window session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ String "window-1"


{- Close Window -}

delete_session_id_window
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
delete_session_id_window session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ toJSONList [String "window-1"]


{- Switch To Window -}

post_session_id_window
  :: String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_window session_id payload = do
  verifyIsActiveSession session_id
  return _success_with_empty_object


{- New Window -}

post_session_id_window_new
  :: String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_window_new session_id payload = do
  verifyIsActiveSession session_id
  st <- getMockServer
  case _load_page "about:blank" st of -- not actually switching contexts here
    Nothing -> errorMockNetwork _err_unknown_error
    Just _st -> do
      modifyMockServer (const _st)
      return $ _success_with_value $ object
        [ ( "handle", String "handle-id" )
        , ( "type", String "tab" )
        ]


{- Get Window Handles -}

get_session_id_window_handles
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_window_handles session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ toJSONList [ String "handle-id" ]


{- Switch To Frame -}

post_session_id_frame
  :: String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_frame session_id !payload = do
  verifyIsActiveSession session_id
  return _success_with_empty_object


{- Switch To Parent Frame -}

post_session_id_frame_parent
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_frame_parent session_id = do
  verifyIsActiveSession session_id
  return _success_with_empty_object


{- Get Window Rect -}

get_session_id_window_rect
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_window_rect session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ object
    [ ("x", Number 0)
    , ("y", Number 0)
    , ("height", Number 480)
    , ("width", Number 640)
    ]


{- Set Window Rect -}

post_session_id_window_rect
  :: String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_window_rect session_id payload = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ object
    [ ("x", Number 0)
    , ("y", Number 0)
    , ("height", Number 480)
    , ("width", Number 640)
    ]


{- Maximize Window -}

post_session_id_window_maximize
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_window_maximize !session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ object
    [ ("x", Number 0)
    , ("y", Number 0)
    , ("height", Number 480)
    , ("width", Number 640)
    ]


{- Minimize Window -}

post_session_id_window_minimize
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_window_minimize !session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ object
    [ ("x", Number 0)
    , ("y", Number 0)
    , ("height", Number 0)
    , ("width", Number 0)
    ]


{- Fullscreen Window -}

post_session_id_window_fullscreen
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_window_fullscreen !session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ object
    [ ("x", Number 0)
    , ("y", Number 0)
    , ("height", Number 480)
    , ("width", Number 640)
    ]


{- Find Element -}

post_session_id_element
  :: String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_element session_id payload = do
  verifyIsActiveSession session_id
  strategy <- getAProperty "using" payload
  case strategy of
    "css selector" -> do
      val <- getAProperty "value" payload
      sel <- parseCssSelector val
      doc <- fmap (contents . _current_page) getMockServer
      case cssMatchDocument sel doc of
        [] -> errorMockNetwork _err_no_such_element
        (d:_) -> do
          return $ _success_with_value $ object
            [ ("element-6066-11e4-a52e-4f735466cecf", String (pack $ elementId d))
            ]
    _ -> do
      return $ _success_with_value $ object
        [ ("element-6066-11e4-a52e-4f735466cecf", String "element-id")
        ]


{- Find Elements -}

post_session_id_elements
  :: String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_elements session_id payload = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ toJSONList [object
    [ ("element-6066-11e4-a52e-4f735466cecf", String "element-id")
    ]]


{- Find Element From Element -}

post_session_id_element_id_element
  :: String
  -> String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_element_id_element session_id element_id payload = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ object
      [ ("element-6066-11e4-a52e-4f735466cecf", String "element-id")
      ]


{- Find Elements From Element -}

post_session_id_element_id_elements
  :: String
  -> String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_element_id_elements session_id element_id payload = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ toJSONList [object
    [ ("element-6066-11e4-a52e-4f735466cecf", String "element-id")
    ]]


{- Get Active Element -}

get_session_id_element_active
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_element_active session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ object
    [ ("element-6066-11e4-a52e-4f735466cecf", String "element-id")
    ]


{- Is Element Selected -}

get_session_id_element_id_selected
  :: String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_element_id_selected session_id element_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ Bool True


{- Get Element Attribute -}

get_session_id_element_id_attribute_name
  :: String
  -> String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_element_id_attribute_name session_id element_id name = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ String "foo"

{- TODO: get_session_id_element_id_property_name -}

get_session_id_element_id_css_name
  :: String
  -> String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_element_id_css_name session_id element_id name = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ String "none"


{- Get Element Text -}

get_session_id_element_id_text
  :: String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_element_id_text session_id element_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ String "foo"


{- Get Element Tag Name -}

get_session_id_element_id_name
  :: String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_element_id_name session_id element_id = do
  verifyIsActiveSession session_id
  doc <- getElementFromId element_id
  return $ _success_with_value $ String (pack $ show $ tag doc)


{- Get Element Rect -}

get_session_id_element_id_rect
  :: String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_element_id_rect !session_id !element_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ object
    [ ("x", Number 0)
    , ("y", Number 0)
    , ("height", Number 48)
    , ("width", Number 64)
    ]

get_session_id_element_id_enabled
  :: String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_element_id_enabled session_id element_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ Bool True


{- Get Computed Role -}

get_session_id_element_id_computedrole
  :: String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_element_id_computedrole session_id element_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ String "textbox"


{- Get Computed Label -}

get_session_id_element_id_computedlabel
  :: String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_element_id_computedlabel session_id element_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ String "textbox"


post_session_id_element_id_click
  :: String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_element_id_click session_id element_id = do
  verifyIsActiveSession session_id
  return _success_with_empty_object


{- Element Clear -}

post_session_id_element_id_clear
  :: String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_element_id_clear session_id element_id = do
  verifyIsActiveSession session_id
  doc <- getElementFromId element_id
  case doc of
    Text _ -> errorMockNetwork _err_invalid_element_state
    d -> if tagIsClearable (tag d)
      then return _success_with_empty_object
      else errorMockNetwork _err_invalid_element_state


{- Element Send Keys -}

post_session_id_element_id_value
  :: String
  -> String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_element_id_value session_id element_id payload = do
  verifyIsActiveSession session_id
  return _success_with_empty_object


{- Get Page Source -}

get_session_id_source
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_source session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ String "the source"

{- TODO: post_session_id_execute_sync -}

{- TODO: post_session_id_execute_async -}


{- Get All Cookies -}

get_session_id_cookie
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_cookie session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ Array $ V.fromList []


{- Get Named Cookie -}

get_session_id_cookie_name
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_cookie_name session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ toJSON $ emptyCookie
      { _cookieName = Just "fakeCookie"
      , _cookieValue = Just "fakeValue"
      }

post_session_id_cookie
  :: String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_cookie session_id payload = do
  verifyIsActiveSession session_id
  return _success_with_empty_object

delete_session_id_cookie_name
  :: String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
delete_session_id_cookie_name session_id name = do
  verifyIsActiveSession session_id
  return _success_with_empty_object

delete_session_id_cookie
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
delete_session_id_cookie session_id = do
  verifyIsActiveSession session_id
  return _success_with_empty_object


{- Perform Actions -}

post_session_id_actions
  :: String
  -> LB.ByteString
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_actions !session_id payload = do
  verifyIsActiveSession session_id
  return _success_with_empty_object

delete_session_id_actions
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
delete_session_id_actions !session_id = do
  verifyIsActiveSession session_id
  return _success_with_empty_object


{- Dismiss Alert -}

post_session_id_alert_dismiss
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_alert_dismiss session_id = do
  verifyIsActiveSession session_id
  return _success_with_empty_object


{- Accept Alert -}

post_session_id_alert_accept
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_alert_accept session_id = do
  verifyIsActiveSession session_id
  return _success_with_empty_object


{- Get Alert Text -}

get_session_id_alert_text
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_alert_text session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ String "WOO!!"


{- Print Page -}

post_session_id_print
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_print session_id = do
  verifyIsActiveSession session_id
  return $ _success_with_value $ String "WOO!!"


{- Send Alert Text -}

post_session_id_alert_text
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
post_session_id_alert_text session_id = do
  verifyIsActiveSession session_id
  return _success_with_empty_object


{- Take Screenshot -}

get_session_id_screenshot
  :: String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_screenshot !session_id = do
  let
    black :: Int -> Int -> PixelRGB8
    black _ _ = PixelRGB8 0 0 0

    img :: DynamicImage
    img = ImageRGB8 (generateImage black 640 480)

    resp :: Text
    resp = decodeUtf8 $ B64.encode $ LB.toStrict $ imageToPng img

  verifyIsActiveSession session_id
  return $ _success_with_value $ String resp


{- Take Element Screenshot -}

get_session_id_element_id_screenshot
  :: String
  -> String
  -> MockNetwork WebDriverServerState HttpResponse
get_session_id_element_id_screenshot session_id element_id = do
  let
    black :: Int -> Int -> PixelRGB8
    black _ _ = PixelRGB8 0 0 0

    img :: DynamicImage
    img = ImageRGB8 (generateImage black 64 48)

    resp :: Text
    resp = decodeUtf8 $ B64.encode $ LB.toStrict $ imageToPng img

  verifyIsActiveSession session_id
  return $ _success_with_value $ String resp



{---------------------}
{- Success Responses -}
{---------------------}

_success_with_empty_object :: HttpResponse
_success_with_empty_object = _success_with_value $ object []

_success_with_value :: Value -> HttpResponse
_success_with_value v =
  _200ok $ encode $ object [ ("value", v ) ]



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
#if MIN_VERSION_http_client(0,7,8)
  , responseOriginalRequest =
      error "emptyResponse: responseOriginalRequest not defined"
#endif
  }

_err_invalid_argument :: String -> HttpException
_err_invalid_argument msg =
  HttpExceptionRequest undefined $ StatusCodeException
    (emptyResponse { responseStatus = badRequest400 })
    (errorObject "invalid argument" msg "" Nothing)

_err_invalid_element_state :: HttpException
_err_invalid_element_state =
  HttpExceptionRequest undefined $ StatusCodeException
    (emptyResponse { responseStatus = badRequest400 })
    (errorObject "invalid element state" "" "" Nothing)

_err_no_such_element :: HttpException
_err_no_such_element =
  HttpExceptionRequest undefined $ StatusCodeException
    (emptyResponse { responseStatus = notFound404 })
    (errorObject "no such element" "" "" Nothing)

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



parseCssSelector :: String -> MockNetwork WebDriverServerState CssSelector
parseCssSelector str = case parseCss str of
  Left err -> errorMockNetwork (_err_invalid_argument $ show err)
  Right x -> return x

verifyIsActiveSession :: String -> MockNetwork WebDriverServerState ()
verifyIsActiveSession session_id = do
  st <- getMockServer
  if _is_active_session session_id st
    then return ()
    else errorMockNetwork _err_invalid_session_id

getElementFromId :: String -> MockNetwork WebDriverServerState Document
getElementFromId element_id = do
  p <- fmap _current_page getMockServer
  case getElementById element_id p of
    Nothing -> errorMockNetwork _err_no_such_element
    Just doc -> return doc

closeTheSession :: String -> MockNetwork WebDriverServerState ()
closeTheSession session_id =
  modifyMockServer (_delete_session session_id)

getAProperty :: String -> LB.ByteString -> MockNetwork WebDriverServerState String
getAProperty k payload = do
  let v = payload ^? key (pack k) . _String
  case v of
    Nothing -> errorMockNetwork $ _err_invalid_argument (show payload)
    Just value -> return $ unpack value

{- |
Module      : Web.Api.WebDriver.Endpoints
Description : Type-safe bindings for the WebDriver HTTP API.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

These bindings try to stick as closely to the spec as possible. We use the following conventions.

* The @Stealth@ suffix on a function indicates that it does not log request or response data (but it does log that a request/response occurred).
* A prime (@'@) on a POST function name indicates that it takes an additional function parameter that mutates the payload after it is converted to JSON, but before sending the request. This is a cheap way to future-proof the bindings and accommodate nonstandard request parameters.

The most recent version of the spec is available at <https://w3c.github.io/webdriver/webdriver-spec.html>.
-}

{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Web.Api.WebDriver.Endpoints (
    runIsolated

  -- * Sessions
  -- ** New Session
  , newSession
  , newSession'
  -- ** Delete Session
  , deleteSession
  -- ** Status
  , sessionStatus
  -- ** Get Timeouts
  , getTimeouts
  -- ** Set Timeouts
  , setTimeouts

  -- * Navigation
  -- ** Navigate To
  , navigateTo
  , navigateToStealth
  -- ** Get Current URL
  , getCurrentUrl
  -- ** Go Back
  , goBack
  -- ** Go Forward
  , goForward
  -- ** Page Refresh
  , pageRefresh
  -- ** Get Title
  , getTitle

  -- * Command Contexts
  -- ** Get Window Handle
  , getWindowHandle
  -- ** Close Window
  , closeWindow
  -- ** Switch To Window
  , switchToWindow
  -- ** Get Window Handles
  , getWindowHandles
  -- ** Switch To Frame
  , switchToFrame
  -- ** Switch To Parent Frame
  , switchToParentFrame
  -- ** Get Window Rect
  , getWindowRect
  -- ** Set Window Rect
  , setWindowRect
  -- ** Maximize Window
  , maximizeWindow
  -- ** Minimize Window
  , minimizeWindow
  -- ** Fullscreen Window
  , fullscreenWindow

  -- * Element Retrieval
  -- ** Find Element
  , findElement
  -- ** Find Elements
  , findElements
  -- ** Find Element From Element
  , findElementFromElement
  -- ** Find Elements From Element
  , findElementsFromElement
  -- ** Get Active Element
  , getActiveElement

  -- * Element State
  -- ** Is Element Selected
  , isElementSelected
  -- ** Get Element Attribute
  , getElementAttribute
  -- ** Get Element Property
  , getElementProperty
  -- ** Get Element CSS Value
  , getElementCssValue
  -- ** Get Element Text
  , getElementText
  -- ** Get Element Tag Name
  , getElementTagName
  -- ** Get Element Rect
  , getElementRect
  -- ** Is Element Enabled
  , isElementEnabled

  -- * Element Interaction
  -- ** Element Click
  , elementClick
  -- ** Element Clear
  , elementClear
  -- ** Element Send Keys
  , elementSendKeys

  -- * Document Handling
  -- ** Get Page Source
  , getPageSource
  , getPageSourceStealth
  -- ** Execute Script
  , executeScript
  -- ** Execute Async Script
  , executeAsyncScript

  -- * Cookies
  -- ** Get All Cookies
  , getAllCookies
  -- ** Get Named Cookie
  , getNamedCookie
  -- ** Add Cookie
  , addCookie
  -- ** Delete Cookie
  , deleteCookie
  -- ** Delete All Cookies
  , deleteAllCookies

  -- * Actions
  -- ** Perform Actions
  , performActions
  , performActionsStealth
  -- ** Release Actions
  , releaseActions

  -- * User Prompts
  -- ** Dismiss Alert
  , dismissAlert
  -- ** Accept Alert
  , acceptAlert
  -- ** Get Alert Text
  , getAlertText
  -- ** Send Alert Text
  , sendAlertText

  -- * Screen Capture
  -- ** Take Screenshot
  , takeScreenshot
  -- ** Take Element Screenshot
  , takeElementScreenshot

  -- Spec Constants
  , _WEB_ELEMENT_ID
  , _WEB_WINDOW_ID
  , _WEB_FRAME_ID
  ) where

import Data.Aeson
  ( Value(..), encode, object, (.=), toJSON )
import Data.Text
  ( Text, unpack, pack )
import Data.Text.Encoding
  ( encodeUtf8 )
import qualified Data.ByteString as SB
import qualified Data.ByteString.Base64 as B64
import qualified Network.URI.Encode as E

import Web.Api.WebDriver.Types
import Web.Api.WebDriver.Classes
import Web.Api.WebDriver.Monad



-- | Spec-defined "web element identifier" string constant. See <https://w3c.github.io/webdriver/webdriver-spec.html#elements>.
_WEB_ELEMENT_ID :: Text
_WEB_ELEMENT_ID = "element-6066-11e4-a52e-4f735466cecf"

-- | Spec-defined "web window identifier" string constant. See <https://w3c.github.io/webdriver/webdriver-spec.html#command-contexts>.
_WEB_WINDOW_ID :: Text
_WEB_WINDOW_ID =  "window-fcc6-11e5-b4f8-330a88ab9d7f"

-- | Spec-defined "web frame identifier" string constant. See <https://w3c.github.io/webdriver/webdriver-spec.html#command-contexts>.
_WEB_FRAME_ID :: Text
_WEB_FRAME_ID = "frame-075b-4da1-b6ba-e579c2d3230a"





-- | Url of the remote WebDriver server.
theRemoteUrl :: (Monad m) => WebDriverT m String
theRemoteUrl = do
  host <- fromEnv (_remoteHostname . _env)
  port <- fromEnv (_remotePort . _env)
  path <- fromEnv (_remotePath . _env)
  return $ concat [ "http://", host, ":", show port, path]

-- | Url of the remote WebDriver server, with session ID.
theRemoteUrlWithSession :: (Monad m) => WebDriverT m String
theRemoteUrlWithSession = do
  st <- fromState (_sessionId . _userState)
  case st of
    Nothing -> throwError NoSession
    Just session_id -> do
      baseUrl <- theRemoteUrl
      return $ concat [ baseUrl, "/session/", session_id ]

-- | Set the session id of a `WDState`.
setSessionId
  :: Maybe String
  -> S WDState
  -> S WDState
setSessionId x st = st { _userState = (_userState st) { _sessionId = x } }

-- | If a WebDriver session ends without issuing a delete session command, then the server keeps its session state alive. `cleanupOnError` catches errors and ensures that a `deleteSession` request is sent.
cleanupOnError
  :: (Monad m)
  => WebDriverT m a -- ^ `WebDriver` session that may throw errors
  -> WebDriverT m a
cleanupOnError x = catchError x $ \e ->
  deleteSession >> throwError e

-- | Run a WebDriver computation in an isolated browser session. Ensures that the session is closed on the remote end.
runIsolated
  :: (Monad m)
  => Capabilities
  -> WebDriverT m a
  -> WebDriverT m ()
runIsolated caps theSession = cleanupOnError $ do
  session_id <- newSession caps
  modifyState $ setSessionId (Just session_id)
  theSession
  deleteSession
  modifyState $ setSessionId Nothing





-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#new-session>. For an extensible version allowing arbitrary changes to the JSON value representing the `Capabilities` parameter, see `newSession'`.
newSession
  :: (Monad m)
  => Capabilities
  -> WebDriverT m SessionId
newSession = newSession' id


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#new-session>. This generalizes `newSession'` by taking an additional function @Value -> Value@ that is applied to the `Capabilities` parameter after it is converted to JSON, but before it is passed to the HTTP call.
newSession'
  :: (Monad m)
  => (Value -> Value)
  -> Capabilities
  -> WebDriverT m SessionId
newSession' f caps = do
  baseUrl <- theRemoteUrl
  format <- fromEnv (_responseFormat . _env)
  let
    !payload = encode $ f $ object
      [ "capabilities" .= object
        [ "alwaysMatch" .= toJSON caps ]
      , "desiredCapabilities" .= toJSON caps
      ]
  httpPost (baseUrl ++ "/session") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= case format of
          SpecFormat -> lookupKeyJson "value"
          ChromeFormat -> return
    >>= lookupKeyJson "sessionId"
    >>= constructFromJson
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#delete-session>.
deleteSession
  :: (Monad m) => WebDriverT m ()
deleteSession = do
  (baseUrl, format) <- theRequestContext
  httpDelete baseUrl
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#status>.
sessionStatus
  :: (Monad m)
  => WebDriverT m (Bool, String)
sessionStatus = do
  baseUrl <- theRemoteUrl
  format <- fromEnv (_responseFormat . _env)
  r <- httpGet (baseUrl ++ "/status")
    >>= (return . _responseBody)
    >>= parseJson
  ready <- case format of
    SpecFormat ->
      lookupKeyJson "value" r
        >>= lookupKeyJson "ready"
        >>= constructFromJson
    ChromeFormat -> return True
  msg <- case format of
    SpecFormat ->
      lookupKeyJson "value" r
        >>= lookupKeyJson "message"
        >>= constructFromJson
        >>= (return . unpack)
    ChromeFormat -> return "chromedriver is not spec compliant :)"
  return (ready, msg)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-timeouts>.
getTimeouts
  :: (Monad m)
  => WebDriverT m TimeoutConfig
getTimeouts = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/timeouts")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#set-timeouts>.
setTimeouts
  :: (Monad m)
  => TimeoutConfig
  -> WebDriverT m ()
setTimeouts timeouts = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode timeouts
  httpPost (baseUrl ++ "/timeouts") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#navigate-to>. To access this enpoint without logging the request or the result, use `navigateToStealth`.
navigateTo
  :: (Monad m)
  => Url
  -> WebDriverT m ()
navigateTo url = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "url" .= url ]
  httpPost (baseUrl ++ "/url") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#navigate-to>. This function does not log the request or response; if you do want the interaction logged, use `navigateTo`.
navigateToStealth
  :: (Monad m)
  => Url
  -> WebDriverT m ()
navigateToStealth url = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "url" .= url ]
  httpSilentPost (baseUrl ++ "/url") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-current-url>.
getCurrentUrl
  :: (Monad m)
  => WebDriverT m Url
getCurrentUrl = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/url")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#back>.
goBack
  :: (Monad m)
  => WebDriverT m ()
goBack = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/back") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#forward>.
goForward
  :: (Monad m)
  => WebDriverT m ()
goForward = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/forward") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#refresh>.
pageRefresh
  :: (Monad m)
  => WebDriverT m ()
pageRefresh = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/refresh") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-title>.
getTitle
  :: (Monad m)
  => WebDriverT m String
getTitle = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/title")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-window-handle>.
getWindowHandle
  :: (Monad m)
  => WebDriverT m ContextId
getWindowHandle = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/window")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (return . ContextId . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#close-window>.
closeWindow
  :: (Monad m)
  => WebDriverT m [ContextId]
closeWindow = do
  baseUrl <- theRemoteUrlWithSession
  httpDelete (baseUrl ++ "/window")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (sequence . map constructFromJson)
    >>= (return . map (ContextId . unpack))


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#switch-to-window>.
switchToWindow
  :: (Monad m, HasContextId t)
  => t
  -> WebDriverT m ()
switchToWindow t = do
  let contextId = contextIdOf t
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "handle" .= show contextId ]
  httpPost (baseUrl ++ "/window") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-window-handles>.
getWindowHandles
  :: (Monad m)
  => WebDriverT m [ContextId]
getWindowHandles = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/window/handles")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (sequence . map constructFromJson)
    >>= (return . map (ContextId . unpack))


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#switch-to-frame>.
switchToFrame
  :: (Monad m)
  => FrameReference
  -> WebDriverT m ()
switchToFrame ref = do
  (baseUrl, format) <- theRequestContext
  let
    !frame = case ref of
      TopLevelFrame -> Null
      FrameNumber k -> Number $ fromIntegral k
      FrameContainingElement element_id -> String $ pack $ show element_id

    !payload = encode $ object
      [ "id" .= toJSON frame ]

  httpPost (baseUrl ++ "/frame") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= case format of
          SpecFormat -> expect (object [])
          ChromeFormat -> expect Null
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#switch-to-parent-frame>.
switchToParentFrame
  :: (Monad m)
  => WebDriverT m ()
switchToParentFrame = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/frame/parent") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-window-rect>.
getWindowRect
  :: (Monad m)
  => WebDriverT m Rect
getWindowRect = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/window/rect")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#set-window-rect>.
setWindowRect
  :: (Monad m)
  => Rect
  -> WebDriverT m Rect
setWindowRect rect = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode rect
  httpPost (baseUrl ++ "/window/rect") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#maximize-window>.
maximizeWindow
  :: (Monad m)
  => WebDriverT m Rect
maximizeWindow = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/window/maximize") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#minimize-window>.
minimizeWindow
  :: (Monad m)
  => WebDriverT m Rect
minimizeWindow = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/window/minimize") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#fullscreen-window>.
fullscreenWindow
  :: (Monad m)
  => WebDriverT m Rect
fullscreenWindow = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/window/fullscreen") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#find-element>.
findElement
  :: (Monad m)
  => LocationStrategy
  -> Selector
  -> WebDriverT m ElementRef
findElement strategy selector = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "value" .= selector, "using" .= toJSON strategy ]
  httpPost (baseUrl ++ "/element") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= case format of
          SpecFormat -> lookupKeyJson _WEB_ELEMENT_ID
          ChromeFormat -> lookupKeyJson "ELEMENT"
    >>= constructFromJson
    >>= (return . ElementRef . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#find-elements>.
findElements
  :: (Monad m)
  => LocationStrategy
  -> Selector
  -> WebDriverT m [ElementRef]
findElements strategy selector = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "value" .= selector, "using" .= toJSON strategy ]
  httpPost (baseUrl ++ "/elements") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= case format of
          SpecFormat -> mapM (lookupKeyJson _WEB_ELEMENT_ID)
          ChromeFormat -> mapM (lookupKeyJson "ELEMENT")
    >>= mapM constructFromJson
    >>= (return . map (ElementRef . unpack))


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#find-element-from-element>.
findElementFromElement
  :: (Monad m, HasElementRef t)
  => LocationStrategy
  -> Selector
  -> t
  -> WebDriverT m ElementRef
findElementFromElement strategy selector root = do
  (baseUrl, format) <- theRequestContext
  let root_id = elementRefOf root
  let !payload = encode $ object [ "value" .= selector, "using" .= toJSON strategy ]
  httpPost (baseUrl ++ "/element/" ++ show root_id ++ "/element") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= case format of
          SpecFormat -> lookupKeyJson _WEB_ELEMENT_ID
          ChromeFormat -> lookupKeyJson "ELEMENT"
    >>= constructFromJson
    >>= (return . ElementRef . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#find-elements-from-element>.
findElementsFromElement
  :: (Monad m, HasElementRef t)
  => LocationStrategy
  -> Selector
  -> t
  -> WebDriverT m [ElementRef]
findElementsFromElement strategy selector root = do
  (baseUrl, format) <- theRequestContext
  let root_id = elementRefOf root
  let !payload = encode $ object [ "value" .= selector, "using" .= toJSON strategy ]
  httpPost (baseUrl ++ "/element/" ++ show root_id ++ "/elements") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= case format of
          SpecFormat -> mapM (lookupKeyJson _WEB_ELEMENT_ID)
          ChromeFormat -> mapM (lookupKeyJson "ELEMENT")
    >>= mapM constructFromJson
    >>= (return . map (ElementRef . unpack))


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-active-element>.
getActiveElement
  :: (Monad m)
  => WebDriverT m ElementRef
getActiveElement = do
  (baseUrl, format) <- theRequestContext
  httpGet (baseUrl ++ "/element/active")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= case format of
          SpecFormat -> lookupKeyJson _WEB_ELEMENT_ID
          ChromeFormat -> lookupKeyJson "ELEMENT"
    >>= constructFromJson
    >>= (return . ElementRef . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#is-element-selected>.
isElementSelected
  :: (Monad m, HasElementRef t)
  => t
  -> WebDriverT m Bool
isElementSelected element = do
  let elementRef = show $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/element/" ++ elementRef ++ "/selected")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-attribute>.
getElementAttribute
  :: (Monad m, HasElementRef t)
  => AttributeName
  -> t
  -> WebDriverT m (Either Bool String)
getElementAttribute name element = do
  let elementRef = show $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  x <- httpGet (baseUrl ++ "/element/" ++ elementRef ++ "/attribute/" ++ E.encode name)
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
  case x of
    Null -> return (Left False)
    String "true" -> return (Left True)
    String attr -> return (Right $ unpack attr)
    _ -> throwJsonError $ JsonError "Invalid element attribute response"


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-property>.
getElementProperty
  :: (Monad m, HasElementRef t)
  => PropertyName
  -> t
  -> WebDriverT m Value
getElementProperty name element = do
  let elementRef = show $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/element/" ++ elementRef ++ "/property/" ++ E.encode name)
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-css-value>.
getElementCssValue
  :: (Monad m, HasElementRef t)
  => CssPropertyName
  -> t
  -> WebDriverT m String
getElementCssValue name element = do
  let elementRef = show $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/element/" ++ elementRef ++ "/css/" ++ name)
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-text>.
getElementText
  :: (Monad m, HasElementRef t)
  => t
  -> WebDriverT m String
getElementText element = do
  let elementRef = show $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/element/" ++ elementRef ++ "/text")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-tag-name>.
getElementTagName
  :: (Monad m, HasElementRef t)
  => t
  -> WebDriverT m String
getElementTagName element = do
  let elementRef = show $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/element/" ++ elementRef ++ "/name")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-rect>.
getElementRect
  :: (Monad m, HasElementRef t)
  => t
  -> WebDriverT m Rect
getElementRect element = do
  let elementRef = show $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/element/" ++ elementRef ++ "/rect")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#is-element-enabled>.
isElementEnabled
  :: (Monad m, HasElementRef t)
  => t
  -> WebDriverT m Bool
isElementEnabled element = do
  let elementRef = show $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/element/" ++ elementRef ++ "/enabled")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#element-click>.
elementClick
  :: (Monad m, HasElementRef t)
  => t
  -> WebDriverT m ()
elementClick element = do
  (baseUrl, format) <- theRequestContext
  let elementRef = show $ elementRefOf element
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/element/" ++ elementRef ++ "/click") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#element-clear>.
elementClear
  :: (Monad m, HasElementRef t)
  => t
  -> WebDriverT m ()
elementClear element = do
  (baseUrl, format) <- theRequestContext
  let elementRef = show $ elementRefOf element
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/element/" ++ elementRef ++ "/clear") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#element-send-keys>.
elementSendKeys
  :: (Monad m, HasElementRef t)
  => String
  -> t
  -> WebDriverT m ()
elementSendKeys text element = do
  let elementRef = show $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "text" .= text ]
  httpPost (baseUrl ++ "/element/" ++ elementRef ++ "/value") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expect (object [])
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-page-source>.
getPageSource
  :: (Monad m)
  => WebDriverT m String
getPageSource = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/source")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-page-source>. Does not dump the page source into the logs. :)
getPageSourceStealth
  :: (Monad m)
  => WebDriverT m String
getPageSourceStealth = do
  baseUrl <- theRemoteUrlWithSession
  httpSilentGet (baseUrl ++ "/source")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#execute-script>.
executeScript
  :: (Monad m)
  => Script
  -> [Value]
  -> WebDriverT m Value
executeScript script args = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "script" .= script, "args" .= toJSON args ]
  httpPost (baseUrl ++ "/execute/sync") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#execute-async-script>.
executeAsyncScript
  :: (Monad m)
  => Script
  -> [Value]
  -> WebDriverT m Value
executeAsyncScript script args = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "script" .= script, "args" .= toJSON args ]
  httpPost (baseUrl ++ "/execute/async") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-all-cookies>.
getAllCookies
  :: (Monad m)
  => WebDriverT m [Cookie]
getAllCookies = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/cookie")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= mapM constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-named-cookie>.
getNamedCookie
  :: (Monad m)
  => CookieName
  -> WebDriverT m Cookie
getNamedCookie name = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/cookie/" ++ E.encode name)
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#add-cookie>.
addCookie
  :: (Monad m)
  => Cookie
  -> WebDriverT m ()
addCookie cookie = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "cookie" .= cookie ]
  httpSilentPost (baseUrl ++ "/cookie") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#delete-cookie>.
deleteCookie
  :: (Monad m)
  => CookieName
  -> WebDriverT m ()
deleteCookie name = do
  (baseUrl, format) <- theRequestContext
  httpDelete (baseUrl ++ "/cookie/" ++ E.encode name)
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#delete-all-cookies>.
deleteAllCookies
  :: (Monad m)
  => WebDriverT m ()
deleteAllCookies = do
  (baseUrl, format) <- theRequestContext
  httpDelete (baseUrl ++ "/cookie")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#perform-actions>. For a variant on this endpoint that does not log the request and response, see `performActionsStealth`.
performActions
  :: (Monad m)
  => [Action]
  -> WebDriverT m ()
performActions = _performActions False


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#perform-actions>. This function is identical to `performActions` except that it does not log the request or response. Handy if the action includes secret info.
performActionsStealth
  :: (Monad m)
  => [Action]
  -> WebDriverT m ()
performActionsStealth = _performActions True


_performActions
  :: (Monad m)
  => Bool
  -> [Action]
  -> WebDriverT m ()
_performActions stealth action = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "actions" .= toJSON action ]
  let httpMethod = if stealth then httpSilentPost else httpPost
  httpMethod (baseUrl ++ "/actions") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expect (object [])
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#release-actions>.
releaseActions
  :: (Monad m)
  => WebDriverT m ()
releaseActions = do
  baseUrl <- theRemoteUrlWithSession
  httpDelete (baseUrl ++ "/actions")
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dismiss-alert>.
dismissAlert
  :: (Monad m)
  => WebDriverT m ()
dismissAlert = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/alert/dismiss") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#accept-alert>.
acceptAlert
  :: (Monad m)
  => WebDriverT m ()
acceptAlert = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/alert/accept") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-alert-text>.
getAlertText
  :: (Monad m)
  => WebDriverT m (Maybe String)
getAlertText = do
  baseUrl <- theRemoteUrlWithSession
  msg <- httpGet (baseUrl ++ "/alert/text")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
  case msg of
    Null -> return Nothing
    String text -> return $ Just (unpack text)
    _ -> throwJsonError $ JsonError "Invalid alert text response"


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#send-alert-text>.
sendAlertText
  :: (Monad m)
  => String
  -> WebDriverT m ()
sendAlertText msg = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "text" .= msg ]
  httpPost (baseUrl ++ "/alert/text") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#take-screenshot>.
takeScreenshot
  :: (Monad m)
  => WebDriverT m SB.ByteString
takeScreenshot = do
  baseUrl <- theRemoteUrlWithSession
  result <- httpGet (baseUrl ++ "/screenshot")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (return . B64.decode . encodeUtf8)
  case result of
    Right img -> return img
    Left str -> throwError $ ImageDecodeError str


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#take-element-screenshot>.
takeElementScreenshot
  :: (Monad m, HasElementRef t)
  => t
  -> WebDriverT m SB.ByteString
takeElementScreenshot element = do
  let elementRef = show $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  result <- httpGet (baseUrl ++ "/element/" ++ elementRef ++ "/screenshot")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (return . B64.decode . encodeUtf8)
  case result of
    Right img -> return img
    Left str -> throwError $ ImageDecodeError str


-- | Detect empty responses by response format. Necessary because chromedriver is not strictly spec compliant.
expectEmptyObject
  :: (Monad m)
  => ResponseFormat
  -> Value
  -> WebDriverT m Value
expectEmptyObject format value = case format of
  SpecFormat -> expect (object []) value
  ChromeFormat -> expect Null value


theRequestContext
  :: (Monad m)
  => WebDriverT m (String, ResponseFormat)
theRequestContext = do
  baseUrl <- theRemoteUrlWithSession
  format <- fromEnv (_responseFormat . _env)
  return (baseUrl, format)

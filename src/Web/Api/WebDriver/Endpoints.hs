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
  , runIsolated_

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
  -- ** New Window
  , newWindow
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
  -- ** Get Computed Role
  , getComputedRole
  -- ** Get Computed Label
  , getComputedLabel

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

  -- * Print
  -- ** Print Page
  , printPage

  -- Spec Constants
  , _WEB_ELEMENT_ID
  , _WEB_WINDOW_ID
  , _WEB_FRAME_ID
  ) where

import Control.Monad.Trans.Class
  ( MonadTrans(..) )
import Data.Aeson
  ( Value(..), encode, object, (.=), toJSON )
import Data.Aeson.Key
  ( fromText )
import Data.Text
  ( Text, unpack )
import Data.Text.Encoding
  ( encodeUtf8 )
import qualified Data.ByteString as SB
import qualified Data.ByteString.Base64 as B64
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.URI.Encode as E

import Web.Api.WebDriver.Types
import Web.Api.WebDriver.Classes
import Web.Api.WebDriver.Monad



-- | Spec-defined "web element identifier" string constant. See <https://w3c.github.io/webdriver/webdriver-spec.html#elements>.
_WEB_ELEMENT_ID :: (IsString t) => t
_WEB_ELEMENT_ID = "element-6066-11e4-a52e-4f735466cecf"

-- | Spec-defined "web window identifier" string constant. See <https://w3c.github.io/webdriver/webdriver-spec.html#command-contexts>.
_WEB_WINDOW_ID :: (IsString t) => t
_WEB_WINDOW_ID =  "window-fcc6-11e5-b4f8-330a88ab9d7f"

-- | Spec-defined "web frame identifier" string constant. See <https://w3c.github.io/webdriver/webdriver-spec.html#command-contexts>.
_WEB_FRAME_ID :: (IsString t) => t
_WEB_FRAME_ID = "frame-075b-4da1-b6ba-e579c2d3230a"





-- | Url of the remote WebDriver server.
theRemoteUrl
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff Url
theRemoteUrl = do
  host <- fromEnv (_remoteHostname . _env)
  port <- fromEnv (_remotePort . _env)
  path <- fromEnv (_remotePath . _env)
  return $ T.concat [ "http://", host, ":", T.pack $ show port, path]

-- | Url of the remote WebDriver server, with session ID.
theRemoteUrlWithSession :: (Monad eff, Monad (t eff), MonadTrans t) => WebDriverTT t eff Url
theRemoteUrlWithSession = do
  st <- fromState (_sessionId . _userState)
  case st of
    Nothing -> throwError NoSession
    Just session_id -> do
      baseUrl <- theRemoteUrl
      return $ T.concat [ baseUrl, "/session/", session_id ]

-- | Set the session id of a `WDState`.
setSessionId
  :: Maybe Text
  -> S WDState
  -> S WDState
setSessionId x st = st { _userState = (_userState st) { _sessionId = x } }

-- | If a WebDriver session ends without issuing a delete session command, then the server keeps its session state alive. `cleanupOnError` catches errors and ensures that a `deleteSession` request is sent.
cleanupOnError
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff a -- ^ `WebDriver` session that may throw errors
  -> WebDriverTT t eff a
cleanupOnError x = catchAnyError x
  (\e -> deleteSession >> throwError e)
  (\e -> deleteSession >> throwHttpException e)
  (\e -> deleteSession >> throwIOException e)
  (\e -> deleteSession >> throwJsonError e)

-- | Run a WebDriver computation in an isolated browser session. Ensures that the session is closed on the remote end.
runIsolated
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Capabilities
  -> WebDriverTT t eff a
  -> WebDriverTT t eff a
runIsolated caps theSession = cleanupOnError $ do
  session_id <- newSession caps
  modifyState $ setSessionId (Just session_id)
  a <- theSession
  deleteSession
  modifyState $ setSessionId Nothing
  return a

runIsolated_
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Capabilities
  -> WebDriverTT t eff a
  -> WebDriverTT t eff ()
runIsolated_ caps theSession =
  runIsolated caps theSession >> return ()





-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#new-session>. For an extensible version allowing arbitrary changes to the JSON value representing the `Capabilities` parameter, see `newSession'`.
newSession
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Capabilities
  -> WebDriverTT t eff SessionId
newSession = newSession' id


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#new-session>. This generalizes `newSession'` by taking an additional function @Value -> Value@ that is applied to the `Capabilities` parameter after it is converted to JSON, but before it is passed to the HTTP call.
newSession'
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (Value -> Value)
  -> Capabilities
  -> WebDriverTT t eff SessionId
newSession' f caps = do
  baseUrl <- theRemoteUrl
  format <- fromEnv (_responseFormat . _env)
  let
    !payload = encode $ f $ object
      [ "capabilities" .= object
        [ "alwaysMatch" .= toJSON caps ]
      , "desiredCapabilities" .= toJSON caps
      ]
  httpPost (baseUrl <> "/session") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= case format of
          SpecFormat -> lookupKeyJson "value"
          ChromeFormat -> return
    >>= lookupKeyJson "sessionId"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#delete-session>.
deleteSession
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ()
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
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff (Bool, Text)
sessionStatus = do
  baseUrl <- theRemoteUrl
  format <- fromEnv (_responseFormat . _env)
  r <- httpGet (baseUrl <> "/status")
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
    ChromeFormat -> return "chromedriver is not spec compliant :)"
  return (ready, msg)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-timeouts>.
getTimeouts
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff TimeoutConfig
getTimeouts = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/timeouts")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#set-timeouts>.
setTimeouts
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => TimeoutConfig
  -> WebDriverTT t eff ()
setTimeouts timeouts = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode timeouts
  httpPost (baseUrl <> "/timeouts") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#navigate-to>. To access this enpoint without logging the request or the result, use `navigateToStealth`.
navigateTo
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Url
  -> WebDriverTT t eff ()
navigateTo url = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "url" .= url ]
  httpPost (baseUrl <> "/url") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#navigate-to>. This function does not log the request or response; if you do want the interaction logged, use `navigateTo`.
navigateToStealth
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Url
  -> WebDriverTT t eff ()
navigateToStealth url = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "url" .= url ]
  httpSilentPost (baseUrl <> "/url") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-current-url>.
getCurrentUrl
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff Url
getCurrentUrl = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/url")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#back>.
goBack
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ()
goBack = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object []
  httpPost (baseUrl <> "/back") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#forward>.
goForward
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ()
goForward = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object []
  httpPost (baseUrl <> "/forward") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#refresh>.
pageRefresh
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ()
pageRefresh = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object []
  httpPost (baseUrl <> "/refresh") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-title>.
getTitle
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff Text
getTitle = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/title")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-window-handle>.
getWindowHandle
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ContextId
getWindowHandle = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/window")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (return . ContextId)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#close-window>.
closeWindow
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff [ContextId]
closeWindow = do
  baseUrl <- theRemoteUrlWithSession
  httpDelete (baseUrl <> "/window")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (sequence . map constructFromJson)
    >>= (return . map ContextId)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#switch-to-window>.
switchToWindow
  :: (Monad eff, Monad (t eff), MonadTrans t, HasContextId a)
  => a
  -> WebDriverTT t eff ()
switchToWindow t = do
  let contextId = contextIdOf t
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "handle" .= show contextId ]
  httpPost (baseUrl <> "/window") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-window-handles>.
getWindowHandles
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff [ContextId]
getWindowHandles = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/window/handles")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (sequence . map constructFromJson)
    >>= (return . map ContextId)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#new-window>
newWindow
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => ContextType -> WebDriverTT t eff (ContextId, ContextType)
newWindow ctxTypeReq = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "type" .= ctxTypeReq ]
  response <- httpPost (baseUrl <> "/window/new") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
  ctxId <- lookupKeyJson "handle" response
    >>= constructFromJson
  ctxType <- lookupKeyJson "type" response
    >>= constructFromJson
  return (ctxId, ctxType)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#switch-to-frame>.
switchToFrame
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => FrameReference
  -> WebDriverTT t eff ()
switchToFrame ref = do
  (baseUrl, format) <- theRequestContext
  let
    !frame = case ref of
      TopLevelFrame -> Null
      FrameNumber k -> Number $ fromIntegral k
      FrameContainingElement element_id -> object [ (fromText _WEB_ELEMENT_ID) .= show element_id ]

    !payload = encode $ object
      [ "id" .= toJSON frame ]

  httpPost (baseUrl <> "/frame") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#switch-to-parent-frame>.
switchToParentFrame
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ()
switchToParentFrame = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object []
  httpPost (baseUrl <> "/frame/parent") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-window-rect>.
getWindowRect
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff Rect
getWindowRect = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/window/rect")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#set-window-rect>.
setWindowRect
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Rect
  -> WebDriverTT t eff Rect
setWindowRect rect = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode rect
  httpPost (baseUrl <> "/window/rect") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#maximize-window>.
maximizeWindow
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff Rect
maximizeWindow = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl <> "/window/maximize") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#minimize-window>.
minimizeWindow
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff Rect
minimizeWindow = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl <> "/window/minimize") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#fullscreen-window>.
fullscreenWindow
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff Rect
fullscreenWindow = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl <> "/window/fullscreen") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#find-element>.
findElement
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => LocationStrategy
  -> Selector
  -> WebDriverTT t eff ElementRef
findElement strategy selector = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "value" .= selector, "using" .= toJSON strategy ]
  httpPost (baseUrl <> "/element") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= case format of
          SpecFormat -> lookupKeyJson _WEB_ELEMENT_ID
          ChromeFormat -> lookupKeyJson "ELEMENT"
    >>= constructFromJson
    >>= (return . ElementRef)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#find-elements>.
findElements
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => LocationStrategy
  -> Selector
  -> WebDriverTT t eff [ElementRef]
findElements strategy selector = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "value" .= selector, "using" .= toJSON strategy ]
  httpPost (baseUrl <> "/elements") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= case format of
          SpecFormat -> mapM (lookupKeyJson _WEB_ELEMENT_ID)
          ChromeFormat -> mapM (lookupKeyJson "ELEMENT")
    >>= mapM constructFromJson
    >>= (return . map ElementRef)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#find-element-from-element>.
findElementFromElement
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => LocationStrategy
  -> Selector
  -> a
  -> WebDriverTT t eff ElementRef
findElementFromElement strategy selector root = do
  (baseUrl, format) <- theRequestContext
  let root_id = theElementRef $ elementRefOf root
  let !payload = encode $ object [ "value" .= selector, "using" .= toJSON strategy ]
  httpPost (baseUrl <> "/element/" <> root_id <> "/element") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= case format of
          SpecFormat -> lookupKeyJson _WEB_ELEMENT_ID
          ChromeFormat -> lookupKeyJson "ELEMENT"
    >>= constructFromJson
    >>= (return . ElementRef)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#find-elements-from-element>.
findElementsFromElement
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => LocationStrategy
  -> Selector
  -> a
  -> WebDriverTT t eff [ElementRef]
findElementsFromElement strategy selector root = do
  (baseUrl, format) <- theRequestContext
  let root_id = theElementRef $ elementRefOf root
  let !payload = encode $ object [ "value" .= selector, "using" .= toJSON strategy ]
  httpPost (baseUrl <> "/element/" <> root_id <> "/elements") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= case format of
          SpecFormat -> mapM (lookupKeyJson _WEB_ELEMENT_ID)
          ChromeFormat -> mapM (lookupKeyJson "ELEMENT")
    >>= mapM constructFromJson
    >>= (return . map ElementRef)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-active-element>.
getActiveElement
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ElementRef
getActiveElement = do
  (baseUrl, format) <- theRequestContext
  httpGet (baseUrl <> "/element/active")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= case format of
          SpecFormat -> lookupKeyJson _WEB_ELEMENT_ID
          ChromeFormat -> lookupKeyJson "ELEMENT"
    >>= constructFromJson
    >>= (return . ElementRef)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#is-element-selected>.
isElementSelected
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => a
  -> WebDriverTT t eff Bool
isElementSelected element = do
  let elementRef = theElementRef $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/element/" <> elementRef <> "/selected")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-attribute>.
getElementAttribute
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => AttributeName
  -> a
  -> WebDriverTT t eff (Either Bool Text)
getElementAttribute name element = do
  let elementRef = theElementRef $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  x <- httpGet (baseUrl <> "/element/" <> elementRef <> "/attribute/" <> E.encodeText name)
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
  case x of
    Null -> return (Left False)
    String "true" -> return (Left True)
    String attr -> return (Right attr)
    _ -> throwJsonError $ JsonError "Invalid element attribute response"


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-property>.
getElementProperty
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => PropertyName
  -> a
  -> WebDriverTT t eff Value
getElementProperty name element = do
  let elementRef = theElementRef $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/element/" <> elementRef <> "/property/" <> E.encodeText name)
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-css-value>.
getElementCssValue
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => CssPropertyName
  -> a
  -> WebDriverTT t eff Text
getElementCssValue name element = do
  let elementRef = theElementRef $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/element/" <> elementRef <> "/css/" <> name)
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-text>.
getElementText
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => a
  -> WebDriverTT t eff Text
getElementText element = do
  let elementRef = theElementRef $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/element/" <> elementRef <> "/text")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-tag-name>.
getElementTagName
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => a
  -> WebDriverTT t eff Text
getElementTagName element = do
  let elementRef = theElementRef $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/element/" <> elementRef <> "/name")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-rect>.
getElementRect
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => a
  -> WebDriverTT t eff Rect
getElementRect element = do
  let elementRef = theElementRef $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/element/" <> elementRef <> "/rect")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#is-element-enabled>.
isElementEnabled
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => a
  -> WebDriverTT t eff Bool
isElementEnabled element = do
  let elementRef = theElementRef $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/element/" <> elementRef <> "/enabled")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-computed-role>
getComputedRole
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => a
  -> WebDriverTT t eff AriaRole
getComputedRole element = do
  let elementRef = theElementRef $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/element/" <> elementRef <> "/computedrole")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-computed-label>
getComputedLabel
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => a
  -> WebDriverTT t eff AriaLabel
getComputedLabel element = do
  let elementRef = theElementRef $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/element/" <> elementRef <> "/computedlabel")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#element-click>.
elementClick
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => a
  -> WebDriverTT t eff ()
elementClick element = do
  (baseUrl, format) <- theRequestContext
  let elementRef = theElementRef $ elementRefOf element
  let !payload = encode $ object []
  httpPost (baseUrl <> "/element/" <> elementRef <> "/click") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#element-clear>.
elementClear
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => a
  -> WebDriverTT t eff ()
elementClear element = do
  (baseUrl, format) <- theRequestContext
  let elementRef = theElementRef $ elementRefOf element
  let !payload = encode $ object []
  httpPost (baseUrl <> "/element/" <> elementRef <> "/clear") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#element-send-keys>.
elementSendKeys
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => Text
  -> a
  -> WebDriverTT t eff ()
elementSendKeys text element = do
  let elementRef = theElementRef $ elementRefOf element
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "text" .= text ]
  httpPost (baseUrl <> "/element/" <> elementRef <> "/value") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-page-source>.
getPageSource
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff Text
getPageSource = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/source")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-page-source>. Does not dump the page source into the logs. :)
getPageSourceStealth
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff Text
getPageSourceStealth = do
  baseUrl <- theRemoteUrlWithSession
  httpSilentGet (baseUrl <> "/source")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#execute-script>.
executeScript
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Script
  -> [Value]
  -> WebDriverTT t eff Value
executeScript script args = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "script" .= script, "args" .= toJSON args ]
  httpPost (baseUrl <> "/execute/sync") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#execute-async-script>.
executeAsyncScript
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Script
  -> [Value]
  -> WebDriverTT t eff Value
executeAsyncScript script args = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "script" .= script, "args" .= toJSON args ]
  httpPost (baseUrl <> "/execute/async") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-all-cookies>.
getAllCookies
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff [Cookie]
getAllCookies = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/cookie")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= mapM constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-named-cookie>.
getNamedCookie
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => CookieName
  -> WebDriverTT t eff Cookie
getNamedCookie name = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl <> "/cookie/" <> T.pack (E.encode $ unpack name))
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#add-cookie>.
addCookie
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Cookie
  -> WebDriverTT t eff ()
addCookie c = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "cookie" .= c ]
  httpSilentPost (baseUrl <> "/cookie") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#delete-cookie>.
deleteCookie
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => CookieName
  -> WebDriverTT t eff ()
deleteCookie name = do
  (baseUrl, format) <- theRequestContext
  httpDelete (baseUrl <> "/cookie/" <> T.pack (E.encode $ unpack name))
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#delete-all-cookies>.
deleteAllCookies
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ()
deleteAllCookies = do
  (baseUrl, format) <- theRequestContext
  httpDelete (baseUrl <> "/cookie")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#perform-actions>. For a variant on this endpoint that does not log the request and response, see `performActionsStealth`.
performActions
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => [Action]
  -> WebDriverTT t eff ()
performActions = _performActions False


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#perform-actions>. This function is identical to `performActions` except that it does not log the request or response. Handy if the action includes secret info.
performActionsStealth
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => [Action]
  -> WebDriverTT t eff ()
performActionsStealth = _performActions True


_performActions
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Bool
  -> [Action]
  -> WebDriverTT t eff ()
_performActions stealth action = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "actions" .= toJSON action ]
  let httpMethod = if stealth then httpSilentPost else httpPost
  httpMethod (baseUrl <> "/actions") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#release-actions>.
releaseActions
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ()
releaseActions = do
  baseUrl <- theRemoteUrlWithSession
  httpDelete (baseUrl <> "/actions")
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dismiss-alert>.
dismissAlert
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ()
dismissAlert = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object []
  httpPost (baseUrl <> "/alert/dismiss") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#accept-alert>.
acceptAlert
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff ()
acceptAlert = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object []
  httpPost (baseUrl <> "/alert/accept") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-alert-text>.
getAlertText
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff (Maybe Text)
getAlertText = do
  baseUrl <- theRemoteUrlWithSession
  msg <- httpGet (baseUrl <> "/alert/text")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
  case msg of
    Null -> return Nothing
    String text -> return $ Just text
    _ -> throwJsonError $ JsonError "Invalid alert text response"


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#send-alert-text>.
sendAlertText
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Text
  -> WebDriverTT t eff ()
sendAlertText msg = do
  (baseUrl, format) <- theRequestContext
  let !payload = encode $ object [ "text" .= msg ]
  httpPost (baseUrl <> "/alert/text") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= expectEmptyObject format
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#take-screenshot>.
takeScreenshot
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff SB.ByteString
takeScreenshot = do
  baseUrl <- theRemoteUrlWithSession
  result <- httpGet (baseUrl <> "/screenshot")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (return . B64.decode . encodeUtf8)
  case result of
    Right img -> return img
    Left str -> throwError $ ImageDecodeError $ T.pack str


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#take-element-screenshot>.
takeElementScreenshot
  :: (Monad eff, Monad (t eff), MonadTrans t, HasElementRef a)
  => a
  -> WebDriverTT t eff SB.ByteString
takeElementScreenshot element = do
  let elementRef = theElementRef $ elementRefOf element
  baseUrl <- theRemoteUrlWithSession
  result <- httpGet (baseUrl <> "/element/" <> elementRef <> "/screenshot")
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson
    >>= (return . B64.decode . encodeUtf8)
  case result of
    Right img -> return img
    Left str -> throwError $ ImageDecodeError $ T.pack str


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#print-page>. You may also be interested in `decodeBase64EncodedPdf` and `writeBase64EncodedPdf`.
printPage
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => PrintOptions -> WebDriverTT t eff Base64EncodedPdf
printPage opts = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "parameters" .= opts ]
  httpPost (baseUrl <> "/print") payload
    >>= (return . _responseBody)
    >>= parseJson
    >>= lookupKeyJson "value"
    >>= constructFromJson


-- | Detect empty responses by response format. Necessary because chromedriver is not strictly spec compliant.
expectEmptyObject
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => ResponseFormat
  -> Value
  -> WebDriverTT t eff Value
expectEmptyObject format value = case format of
  SpecFormat -> expectIs (\x -> elem x [Null, object []]) "empty object or null" value
  ChromeFormat -> expect Null value


theRequestContext
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => WebDriverTT t eff (Url, ResponseFormat)
theRequestContext = do
  baseUrl <- theRemoteUrlWithSession
  format <- fromEnv (_responseFormat . _env)
  return (baseUrl, format)

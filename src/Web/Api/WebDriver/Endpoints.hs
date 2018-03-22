{- |
Module      : Web.Api.WebDriver.Endpoints
Description : Type-safe bindings for the WebDriver HTTP API.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

These bindings try to stick as closely to the spec as possible. We use the following conventions.

* The @Stealth@ prefix on a function indicates that it does not log request or response data (but it does log that a request/response occurred).
* A prime (@'@) on a POST function name indicates that it takes an additional function parameter that mutates the payload after it is converted to JSON, but before sending the request. This is a cheap way to future-proof the API and accommodate nonstandard request parameters.

The most recent version of the spec is available at <https://w3c.github.io/webdriver/webdriver-spec.html>.
-}

{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Web.Api.WebDriver.Endpoints (
  -- * Sessions
  -- ** New Session
    newSession
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
  -- , switchToFrame
  -- , switchToParentFrame
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
  -- getElementCssValue
  -- getElementText
  -- getElementTagName
  -- getElementRect
  -- isElementEnabled

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
  , performStealthActions
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
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Base64 as B64
import qualified Network.URI.Encode as E

import Web.Api.Http
import Web.Api.WebDriver.Types
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




-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#new-session>. For an extensible version allowing arbitrary changes to the JSON value representing the `Capabilities` parameter, see `newSession'`.
newSession
  :: (Effectful m)
  => Capabilities
  -> WebDriver m SessionId
newSession = newSession' id


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#new-session>. This generalizes `newSession'` by taking an additional function @Value -> Value@ that is applied to the `Capabilities` parameter after it is converted to JSON, but before it is passed to the HTTP call.
newSession'
  :: (Effectful m)
  => (Value -> Value)
  -> Capabilities
  -> WebDriver m SessionId
newSession' f caps = do
  baseUrl <- theRemoteUrl
  format <- readResponseFormat
  let !payload = encode $ f $ toJSON caps
  httpPost (baseUrl ++ "/session") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= case format of
          SpecFormat -> lookupKey "value"
          ChromeFormat -> return
    >>= lookupKey "sessionId"
    >>= constructFromJSON
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#delete-session>.
deleteSession
  :: (Effectful m)
  => WebDriver m ()
deleteSession = do
  baseUrl <- theRemoteUrlWithSession
  httpDelete baseUrl
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= expect (object [])
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#status>.
sessionStatus
 :: (Effectful m)
 => WebDriver m (Bool, String)
sessionStatus = do
  baseUrl <- theRemoteUrl
  r <- httpGet (baseUrl ++ "/status")
    >>= (return . __response_body)
    >>= mParseJson
  ready <- lookupKey "value" r
    >>= lookupKey "ready"
    >>= constructFromJSON
  msg <- lookupKey "value" r
    >>= lookupKey "message"
    >>= constructFromJSON
    >>= (return . unpack)
  return (ready, msg)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-timeouts>.
getTimeouts
  :: (Effectful m)
  => WebDriver m TimeoutConfig
getTimeouts = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/timeouts")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#set-timeouts>.
setTimeouts
  :: (Effectful m)
  => TimeoutConfig
  -> WebDriver m ()
setTimeouts timeouts = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode timeouts
  httpPost (baseUrl ++ "/timeouts") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#navigate-to>. To access this enpoint without logging the request or the result, use `navigateToStealth`.
navigateTo
  :: (Effectful m)
  => Url
  -> WebDriver m ()
navigateTo url = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "url" .= url ]
  httpPost (baseUrl ++ "/url") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= expect (object [])
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#navigate-to>. This function does not log the request or response; if you do want the interaction logged, use `navigateTo`.
navigateToStealth
  :: (Effectful m)
  => Url
  -> WebDriver m ()
navigateToStealth url = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "url" .= url ]
  httpSilentPost (baseUrl ++ "/url") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-current-url>.
getCurrentUrl
  :: (Effectful m)
  => WebDriver m Url
getCurrentUrl = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/url")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#back>.
goBack
  :: (Effectful m)
  => WebDriver m ()
goBack = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/back") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= expect (object [])
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#forward>.
goForward
  :: (Effectful m)
  => WebDriver m ()
goForward = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/forward") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= expect (object [])
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#refresh>.
pageRefresh
  :: (Effectful m)
  => WebDriver m ()
pageRefresh = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/refresh") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= expect (object [])
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-title>.
getTitle
  :: (Effectful m)
  => WebDriver m String
getTitle = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/title")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-window-handle>.
getWindowHandle
  :: (Effectful m)
  => WebDriver m ContextId
getWindowHandle = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/window")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#close-window>.
closeWindow
  :: (Effectful m)
  => WebDriver m [ContextId]
closeWindow = do
  baseUrl <- theRemoteUrlWithSession
  httpDelete (baseUrl ++ "/window")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON
    >>= (sequence . map constructFromJSON)
    >>= (return . map unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#switch-to-window>.
switchToWindow
  :: (Effectful m)
  => ContextId
  -> WebDriver m ()
switchToWindow contextId = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "handle" .= contextId ]
  httpPost (baseUrl ++ "/window") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-window-handles>.
getWindowHandles
  :: (Effectful m)
  => WebDriver m [ContextId]
getWindowHandles = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/window/handles")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON
    >>= (sequence . map constructFromJSON)
    >>= (return . map unpack)


-- TODO: switchToFrame


-- TODO: switchToParentFrame


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-window-rect>.
getWindowRect
  :: (Effectful m)
  => WebDriver m Rect
getWindowRect = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/window/rect")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#set-window-rect>.
setWindowRect
  :: (Effectful m)
  => Rect
  -> WebDriver m Rect
setWindowRect rect = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode rect
  httpPost (baseUrl ++ "/window/rect") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#maximize-window>.
maximizeWindow
  :: (Effectful m)
  => WebDriver m Rect
maximizeWindow = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/window/maximize") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#minimize-window>.
minimizeWindow
  :: (Effectful m)
  => WebDriver m Rect
minimizeWindow = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/window/minimize") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#fullscreen-window>.
fullscreenWindow
  :: (Effectful m)
  => WebDriver m Rect
fullscreenWindow = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/window/fullscreen") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#find-element>.
findElement
  :: (Effectful m)
  => LocationStrategy
  -> Selector
  -> WebDriver m ElementRef
findElement strategy selector = do
  baseUrl <- theRemoteUrlWithSession
  format <- readResponseFormat
  let !payload = encode $ object [ "value" .= selector, "using" .= toJSON strategy ]
  httpPost (baseUrl ++ "/element") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= case format of
          SpecFormat -> lookupKey _WEB_ELEMENT_ID
          ChromeFormat -> lookupKey "ELEMENT"
    >>= constructFromJSON
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#find-elements>.
findElements
  :: (Effectful m)
  => LocationStrategy
  -> Selector
  -> WebDriver m [ElementRef]
findElements strategy selector = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "value" .= selector, "using" .= toJSON strategy ]
  httpPost (baseUrl ++ "/elements") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON
    >>= mapM (lookupKey _WEB_ELEMENT_ID)
    >>= mapM constructFromJSON
    >>= (return . map unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#find-element-from-element>.
findElementFromElement
  :: (Effectful m)
  => LocationStrategy
  -> Selector
  -> ElementRef
  -> WebDriver m ElementRef
findElementFromElement strategy selector root_id = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "value" .= selector, "using" .= toJSON strategy ]
  httpPost (baseUrl ++ "/element/" ++ root_id ++ "/element") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= lookupKey _WEB_ELEMENT_ID
    >>= constructFromJSON
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#find-elements-from-element>.
findElementsFromElement
  :: (Effectful m)
  => LocationStrategy
  -> Selector
  -> ElementRef
  -> WebDriver m [ElementRef]
findElementsFromElement strategy selector root_id = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "value" .= selector, "using" .= toJSON strategy ]
  httpPost (baseUrl ++ "/element/" ++ root_id ++ "/elements") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON
    >>= mapM (lookupKey _WEB_ELEMENT_ID)
    >>= mapM constructFromJSON
    >>= (return . map unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-active-element>.
getActiveElement
  :: (Effectful m)
  => WebDriver m ElementRef
getActiveElement = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/element/active")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= lookupKey _WEB_ELEMENT_ID
    >>= constructFromJSON
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#is-element-selected>.
isElementSelected
  :: (Effectful m)
  => ElementRef
  -> WebDriver m Bool
isElementSelected element_id = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/element/" ++ element_id ++ "/selected")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-attribute>.
getElementAttribute
  :: (Effectful m)
  => ElementRef
  -> AttributeName
  -> WebDriver m (Either Bool String)
getElementAttribute element_id name = do
  baseUrl <- theRemoteUrlWithSession
  x <- httpGet (baseUrl ++ "/element/" ++ element_id ++ "/attribute/" ++ E.encode name)
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
  case x of
    Null -> return (Left False)
    String "true" -> return (Left True)
    String attr -> return (Right $ unpack attr)
    _ -> mRaiseJsonError JsonError


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-property>.
getElementProperty
  :: (Effectful m)
  => ElementRef
  -> PropertyName
  -> WebDriver m Value
getElementProperty element_id name = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/element/" ++ element_id ++ "/property/" ++ E.encode name)
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"


-- TODO: getElementCssValue


-- TODO: getElementText


-- TODO: getElementTagName


-- TODO: getElementRect


-- TODO: isElementEnabled


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#element-click>.
elementClick
  :: (Effectful m)
  => ElementRef
  -> WebDriver m ()
elementClick element_id = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/element/" ++ element_id ++ "/click") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#element-clear>.
elementClear
  :: (Effectful m)
  => ElementRef
  -> WebDriver m ()
elementClear element_id = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/element/" ++ element_id ++ "/clear") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#element-send-keys>.
elementSendKeys
  :: (Effectful m)
  => ElementRef
  -> String
  -> WebDriver m ()
elementSendKeys element_id text = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "text" .= text ]
  httpPost (baseUrl ++ "/element/" ++ element_id ++ "/value") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-page-source>.
getPageSource
  :: (Effectful m)
  => WebDriver m String
getPageSource = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/source")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON
    >>= (return . unpack)


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#execute-script>.
executeScript
  :: (Effectful m)
  => Script
  -> [Value]
  -> WebDriver m Value
executeScript script args = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "script" .= script, "args" .= toJSON args ]
  httpPost (baseUrl ++ "/execute/sync") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#execute-async-script>.
executeAsyncScript
  :: (Effectful m)
  => Script
  -> [Value]
  -> WebDriver m Value
executeAsyncScript script args = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "script" .= script, "args" .= toJSON args ]
  httpPost (baseUrl ++ "/execute/async") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-all-cookies>.
getAllCookies
  :: (Effectful m)
  => WebDriver m [Cookie]
getAllCookies = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/cookie")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON
    >>= mapM constructFromJSON


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-named-cookie>.
getNamedCookie
  :: (Effectful m)
  => CookieName
  -> WebDriver m Cookie
getNamedCookie name = do
  baseUrl <- theRemoteUrlWithSession
  httpGet (baseUrl ++ "/cookie/" ++ E.encode name)
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#add-cookie>.
addCookie
  :: (Effectful m)
  => Cookie
  -> WebDriver m ()
addCookie cookie = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "cookie" .= cookie ]
  httpSilentPost (baseUrl ++ "/cookie") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#delete-cookie>.
deleteCookie
  :: (Effectful m)
  => CookieName
  -> WebDriver m ()
deleteCookie name = do
  baseUrl <- theRemoteUrlWithSession
  httpDelete (baseUrl ++ "/cookie/" ++ E.encode name)
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#delete-all-cookies>.
deleteAllCookies
  :: (Effectful m)
  => WebDriver m ()
deleteAllCookies = do
  baseUrl <- theRemoteUrlWithSession
  httpDelete (baseUrl ++ "/cookie")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= expect (object [])
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#perform-actions>. For a variant on this endpoint that does not log the request and response, see `performActionsStealth`.
performActions
  :: (Effectful m)
  => [Action]
  -> WebDriver m ()
performActions = _performActions False


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#perform-actions>. This function is identical to `performActions` except that it does not log the request or response. Handy if the action includes secret info.
performStealthActions
  :: (Effectful m)
  => [Action]
  -> WebDriver m ()
performStealthActions = _performActions True


_performActions
  :: (Effectful m)
  => Bool
  -> [Action]
  -> WebDriver m ()
_performActions stealth action = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "actions" .= toJSON action ]
  let httpMethod = if stealth then httpSilentPost else httpPost
  httpMethod (baseUrl ++ "/actions") payload
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= expect (object [])
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#release-actions>.
releaseActions
  :: (Effectful m)
  => WebDriver m ()
releaseActions = do
  baseUrl <- theRemoteUrlWithSession
  httpDelete (baseUrl ++ "/actions")
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dismiss-alert>.
dismissAlert
  :: (Effectful m)
  => WebDriver m ()
dismissAlert = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/alert/dismiss") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#accept-alert>.
acceptAlert
  :: (Effectful m)
  => WebDriver m ()
acceptAlert = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object []
  httpPost (baseUrl ++ "/alert/accept") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-alert-text>.
getAlertText
  :: (Effectful m)
  => WebDriver m (Maybe String)
getAlertText = do
  baseUrl <- theRemoteUrlWithSession
  msg <- httpGet (baseUrl ++ "/alert/text")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
  case msg of
    Null -> return Nothing
    String text -> return $ Just (unpack text)
    _ -> mRaiseJsonError JsonError


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#send-alert-text>.
sendAlertText
  :: (Effectful m)
  => String
  -> WebDriver m ()
sendAlertText msg = do
  baseUrl <- theRemoteUrlWithSession
  let !payload = encode $ object [ "text" .= msg ]
  httpPost (baseUrl ++ "/alert/text") payload
  return ()


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#take-screenshot>.
takeScreenshot
  :: (Effectful m)
  => WebDriver m SB.ByteString
takeScreenshot = do
  baseUrl <- theRemoteUrlWithSession
  result <- httpGet (baseUrl ++ "/screenshot")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON
    >>= (return . B64.decode . encodeUtf8)
  case result of
    Right img -> return img
    Left str -> throwError $ Err $ ImageDecodeError str


-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#take-element-screenshot>.
takeElementScreenshot
  :: (Effectful m)
  => ElementRef
  -> WebDriver m SB.ByteString
takeElementScreenshot element_id = do
  baseUrl <- theRemoteUrlWithSession
  result <- httpGet (baseUrl ++ "/element/" ++ element_id ++ "/screenshot")
    >>= (return . __response_body)
    >>= mParseJson
    >>= lookupKey "value"
    >>= constructFromJSON
    >>= (return . B64.decode . encodeUtf8)
  case result of
    Right img -> return img
    Left str -> throwError $ Err $ ImageDecodeError str

{- |
Module      : Web.Api.WebDriver.Types
Description : Typed arguments for WebDriver endpoints.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

The WebDriver protocol involves passing several different kinds of JSON objects. We can encode these as /types/ to make our DSL more robust; this module is a grab bag of these types. For each one we need `ToJSON` and `FromJSON` instances, and sometimes also a default value.

Note that while the WebDriver spec defines some JSON objects, in general a given WebDriver server can accept additional properties on any given object. Our types here will be limited to the "spec" object signatures, but our API will need to be user extensible.
-}

{-# LANGUAGE OverloadedStrings, RecordWildCards, BangPatterns #-}
module Web.Api.WebDriver.Types (
  -- * Stringy Types
    SessionId
  , ElementRef
  , ContextId
  , Selector
  , AttributeName
  , PropertyName
  , Script
  , CookieName
  , CssPropertyName

  , FrameReference(..)

  -- * Capabilities
  , Capabilities(..)
  , BrowserName(..)
  , PlatformName(..)
  , emptyCapabilities
  , defaultFirefoxCapabilities
  , headlessFirefoxCapabilities
  , defaultChromeCapabilities
  , FirefoxOptions(..)
  , defaultFirefoxOptions
  , ChromeOptions(..)
  , defaultChromeOptions

  -- * Proxy
  , ProxyConfig(..)
  , emptyProxyConfig
  , ProxyType(..)
  , HostAndOptionalPort(..)

  -- * Timeout
  , TimeoutConfig(..)
  , emptyTimeoutConfig

  -- * Input and Actions
  , InputSource(..)
  , PointerSubtype(..)
  , InputSourceParameter(..)
  , Action(..)
  , emptyAction
  , ActionType(..)
  , ActionItem(..)
  , emptyActionItem

  -- * Misc
  , LocationStrategy(..)
  , Rect(..)
  , emptyRect
  , PromptHandler(..)
  , Cookie(..)
  , emptyCookie

  -- * Error Responses
  , ResponseErrorCode(..)
  ) where

import Data.Maybe
  ( catMaybes )
import Data.Char
  ( toLower )
import Data.Text
  ( Text, pack, unpack )
import Data.Scientific
  ( Scientific, scientific )
import Data.Aeson.Types
  ( ToJSON(..), FromJSON(..), Value(..), KeyValue
  , Pair, (.:?), (.:), (.=), object, typeMismatch )
import Test.QuickCheck
  ( Arbitrary(..), arbitraryBoundedEnum, Gen )
import Test.QuickCheck.Gen
  ( listOf, oneof )

import Web.Api.Http.Uri
import Web.Api.WebDriver.Types.Keyboard



unrecognizedValue :: (Monad m) => String -> Text -> m a
unrecognizedValue !name !string = fail $
  "Unrecognized value for type " ++ name ++ ": " ++ unpack string

malformedValue :: (Monad m) => String -> String -> m a
malformedValue !name !value = fail $
  "Malformed value for type" ++ name ++ ": " ++ value



object_ :: [Maybe Pair] -> Value
object_ = object . filter (\(_, v) -> v /= Null) . catMaybes

(.==) :: (ToJSON v, KeyValue kv) => Text -> v -> Maybe kv
(.==) key value = Just (key .= value)

(.=?) :: (ToJSON v, KeyValue kv) => Text -> Maybe v -> Maybe kv
(.=?) key = fmap $ (key .=)



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-session-id>.
type SessionId = String

-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-web-element-reference>.
type ElementRef = String

-- | Identifier for a /browsing context/; see <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-current-browsing-context>.
type ContextId = String

-- | For use with a /Locator Strategy/. See <https://w3c.github.io/webdriver/webdriver-spec.html#locator-strategies>.
type Selector = String

-- | Used with `getElementAttribute`.
type AttributeName = String

-- | Used with `getElementProperty`.
type PropertyName = String

-- | Javascript
type Script = String

-- | Used with `getNamedCookie`.
type CookieName = String

-- | Used with `getElementCssValue`.
type CssPropertyName = String

-- | Possible frame references; see <https://w3c.github.io/webdriver/webdriver-spec.html#switch-to-frame>.
data FrameReference
  = TopLevelFrame
  | FrameNumber Int
  | FrameContainingElement ElementRef
  deriving (Eq, Show)



-- | Semantic HTTP error responses. See <https://w3c.github.io/webdriver/webdriver-spec.html#locator-strategies>.
data ResponseErrorCode
  = ElementClickIntercepted
  | ElementNotSelectable
  | ElementNotInteractable
  | InsecureCertificate
  | InvalidArgument
  | InvalidCookieDomain
  | InvalidCoordinates
  | InvalidElementState
  | InvalidSelector
  | InvalidSessionId
  | JavaScriptError
  | MoveTargetOutOfBounds
  | NoSuchAlert
  | NoSuchCookie
  | NoSuchElement
  | NoSuchFrame
  | NoSuchWindow
  | ScriptTimeout
  | SessionNotCreated
  | StaleElementReference
  | Timeout
  | UnableToSetCookie
  | UnableToCaptureScreen
  | UnexpectedAlertOpen
  | UnknownCommand
  | UnknownError
  | UnknownMethod
  | UnsupportedOperation

  -- | Just in case!
  | UnhandledErrorCode Text
  deriving (Eq, Show)

instance FromJSON ResponseErrorCode where
  parseJSON (String x) = case x of
    "element click intercepted" -> return ElementClickIntercepted
    "element not selectable" -> return ElementNotSelectable
    "element not interactable" -> return ElementNotInteractable
    "insecure certificate" -> return InsecureCertificate
    "invalid argument" -> return InvalidArgument
    "invalid cookie domain" -> return InvalidCookieDomain
    "invalid coordinates" -> return InvalidCoordinates
    "invalid element state" -> return InvalidElementState
    "invalid selector" -> return InvalidSelector
    "invalid session id" -> return InvalidSessionId
    "javascript error" -> return JavaScriptError
    "move target out of bounds" -> return MoveTargetOutOfBounds
    "no such alert" -> return NoSuchAlert
    "no such cookie" -> return NoSuchCookie
    "no such element" -> return NoSuchElement
    "no such frame" -> return NoSuchFrame
    "no such window" -> return NoSuchWindow
    "script timeout" -> return ScriptTimeout
    "session not created" -> return SessionNotCreated
    "stale element reference" -> return StaleElementReference
    "timeout" -> return Timeout
    "unable to set cookie" -> return UnableToSetCookie
    "unable to capture screen" -> return UnableToCaptureScreen
    "unexpected alert open" -> return UnexpectedAlertOpen
    "unknown command" -> return UnknownCommand
    "unknown error" -> return UnknownError
    "unknown method" -> return UnknownMethod
    "unsupported operation" -> return UnsupportedOperation
    text -> return $ UnhandledErrorCode text
  parseJSON invalid = typeMismatch "ResponseErrorCode" invalid

instance ToJSON ResponseErrorCode where
  toJSON x = case x of
    ElementClickIntercepted -> String "element click intercepted"
    ElementNotSelectable -> String "element not selectable"
    ElementNotInteractable -> String "element not interactable"
    InsecureCertificate -> String "insecure certificate"
    InvalidArgument -> String "invalid argument"
    InvalidCookieDomain -> String "invalid cookie domain"
    InvalidCoordinates -> String "invalid coordinates"
    InvalidElementState -> String "invalid element state"
    InvalidSelector -> String "invalid selector"
    InvalidSessionId -> String "invalid session id"
    JavaScriptError -> String "javascript error"
    MoveTargetOutOfBounds -> String "move target out of bounds"
    NoSuchAlert -> String "no such alert"
    NoSuchCookie -> String "no such cookie"
    NoSuchElement -> String "no such element"
    NoSuchFrame -> String "no such frame"
    NoSuchWindow -> String "no such window"
    ScriptTimeout -> String "script timeout"
    SessionNotCreated -> String "session not created"
    StaleElementReference -> String "stale element reference"
    Timeout -> String "timeout"
    UnableToSetCookie -> String "unable to set cookie"
    UnableToCaptureScreen -> String "unable to capture screen"
    UnexpectedAlertOpen -> String "unexpected alert open"
    UnknownCommand -> String "unknown command"
    UnknownError -> String "unknown error"
    UnknownMethod -> String "unknown method"
    UnsupportedOperation -> String "unsupported operation"
    UnhandledErrorCode msg -> String msg

instance Arbitrary ResponseErrorCode where
  arbitrary = oneof $ map return
    [ ElementClickIntercepted
    , ElementNotSelectable
    , ElementNotInteractable
    , InsecureCertificate
    , InvalidArgument
    , InvalidCookieDomain
    , InvalidCoordinates
    , InvalidElementState
    , InvalidSelector
    , InvalidSessionId
    , JavaScriptError
    , MoveTargetOutOfBounds
    , NoSuchAlert
    , NoSuchCookie
    , NoSuchElement
    , NoSuchFrame
    , NoSuchWindow
    , ScriptTimeout
    , SessionNotCreated
    , StaleElementReference
    , Timeout
    , UnableToSetCookie
    , UnableToCaptureScreen
    , UnexpectedAlertOpen
    , UnknownCommand
    , UnknownError
    , UnknownMethod
    , UnsupportedOperation
    ]



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#capabilities>.
data Capabilities = Capabilities
  { _browser_name :: Maybe BrowserName -- ^ @browserName@
  , _browser_version :: Maybe String -- ^ @browserVersion@
  , _platform_name :: Maybe PlatformName -- ^ @platformName@
  , _accept_insecure_certs :: Maybe Bool -- ^ @acceptInsecureCerts@
  , _page_load_strategy :: Maybe String -- ^ @pageLoadStrategy@
  , _proxy :: Maybe ProxyConfig -- ^ @proxy@
  , _set_window_rect :: Maybe Bool -- ^ @setWindowRect@
  , _timeouts :: Maybe TimeoutConfig -- ^ @timeouts@
  , _unhandled_prompt_behavior :: Maybe PromptHandler -- ^ @unhandledPromptBehavior@

  -- | Optional extension, but very common.
  , _chrome_options :: Maybe ChromeOptions -- ^ @chromeOptions@

  -- | Optional extension, but very common.
  , _firefox_options :: Maybe FirefoxOptions -- ^ @moz:firefoxOptions@
  } deriving (Eq, Show)

instance FromJSON Capabilities where
  parseJSON (Object v) = Capabilities
    <$> v .:? "browserName"
    <*> v .:? "browserVersion"
    <*> v .:? "platformName"
    <*> v .:? "acceptInsecureCerts"
    <*> v .:? "pageLoadStrategy"
    <*> v .:? "proxy"
    <*> v .:? "setWindowRect"
    <*> v .:? "timeouts"
    <*> v .:? "unhandledPromptBehavior"
    <*> v .:? "chromeOptions"
    <*> v .:? "moz:firefoxOptions"
  parseJSON invalid = typeMismatch "Capabilities" invalid

instance ToJSON Capabilities where
  toJSON Capabilities{..} = object_
    [ "browserName" .=? (toJSON <$> _browser_name)
    , "browserVersion" .=? (toJSON <$> _browser_version)
    , "platformName" .=? (toJSON <$> _platform_name)
    , "acceptInsecureCerts" .=? (toJSON <$> _accept_insecure_certs)
    , "pageLoadStrategy" .=? (toJSON <$> _page_load_strategy)
    , "proxy" .=? (toJSON <$> _proxy)
    , "setWindowRect" .=? (toJSON <$> _set_window_rect)
    , "timeouts" .=? (toJSON <$> _timeouts)
    , "unhandledPromptBehavior" .=? (toJSON <$> _unhandled_prompt_behavior)
    , "chromeOptions" .=? (toJSON <$> _chrome_options)
    , "moz:firefoxOptions" .=? (toJSON <$> _firefox_options)
    ]

instance Arbitrary Capabilities where
  arbitrary = Capabilities
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

-- | `Capabilities` with all members set to `Nothing`.
emptyCapabilities :: Capabilities
emptyCapabilities = Capabilities
  { _browser_name = Nothing
  , _browser_version = Nothing
  , _platform_name = Nothing
  , _accept_insecure_certs = Nothing
  , _page_load_strategy = Nothing
  , _proxy = Nothing
  , _set_window_rect = Nothing
  , _timeouts = Nothing
  , _unhandled_prompt_behavior = Nothing
  , _chrome_options = Nothing
  , _firefox_options = Nothing
  }

-- | All members set to `Nothing` except `_browser_name`, which is @Just Firefox@.
defaultFirefoxCapabilities :: Capabilities
defaultFirefoxCapabilities = emptyCapabilities
  { _browser_name = Just Firefox
  }

-- | Passing the "-headless" parameter to Firefox.
headlessFirefoxCapabilities :: Capabilities
headlessFirefoxCapabilities = defaultFirefoxCapabilities
  { _firefox_options = Just $ defaultFirefoxOptions
    { _firefox_args = Just ["-headless"]
    }
  }

-- | All members set to `Nothing` except `_browser_name`, which is @Just Chrome@.
defaultChromeCapabilities :: Capabilities
defaultChromeCapabilities = emptyCapabilities
  { _browser_name = Just Chrome
  }



-- | Used in `Capabilities`.
data BrowserName
  = Firefox
  | Chrome
  | Safari
  deriving (Eq, Show, Enum, Bounded)

instance FromJSON BrowserName where
  parseJSON (String x) = case x of
    "firefox" -> return Firefox
    "chrome" -> return Chrome
    "safari" -> return Safari
    _ -> unrecognizedValue "BrowserName" x
  parseJSON invalid = typeMismatch "BrowserName" invalid

instance ToJSON BrowserName where
  toJSON Firefox = String "firefox"
  toJSON Chrome = String "chrome"
  toJSON Safari = String "safari"

instance Arbitrary BrowserName where
  arbitrary = arbitraryBoundedEnum



-- | Used in `Capabilities`.
data PlatformName
  = Mac
  deriving (Eq, Show, Enum, Bounded)

instance FromJSON PlatformName where
  parseJSON (String x) = case unpack x of
    "mac" -> return Mac
    _ -> unrecognizedValue "PlaformName" x
  parseJSON invalid = typeMismatch "PlatformName" invalid

instance ToJSON PlatformName where
  toJSON Mac = String "mac"

instance Arbitrary PlatformName where
  arbitrary = arbitraryBoundedEnum



-- | See <https://sites.google.com/a/chromium.org/chromedriver/capabilities>.
data ChromeOptions = ChromeOptions
  { _chrome_binary :: Maybe String -- ^ @binary@
  , _chrome_args :: Maybe [String] -- ^ @args@
  } deriving (Eq, Show)

instance FromJSON ChromeOptions where
  parseJSON (Object v) = ChromeOptions
    <$> v .:? "binary"
    <*> v .:? "args"
  parseJSON invalid = typeMismatch "ChromeOptions" invalid

instance ToJSON ChromeOptions where
  toJSON ChromeOptions{..} = object_
    [ "binary" .=? (toJSON <$> _chrome_binary)
    , "args" .=? (toJSON <$> _chrome_args)
    ]

instance Arbitrary ChromeOptions where
  arbitrary = ChromeOptions
    <$> arbitrary
    <*> arbitrary

-- | All members set to `Nothing`.
defaultChromeOptions :: ChromeOptions
defaultChromeOptions = ChromeOptions
  { _chrome_binary = Nothing
  , _chrome_args = Nothing
  }



-- | See <https://github.com/mozilla/geckodriver#firefox-capabilities>.
data FirefoxOptions = FirefoxOptions
  { _firefox_binary :: Maybe String -- ^ @binary@
  , _firefox_args :: Maybe [String] -- ^ @args@
  } deriving (Eq, Show)

instance FromJSON FirefoxOptions where
  parseJSON (Object v) = FirefoxOptions
    <$> v .:? "binary"
    <*> v .:? "args"
  parseJSON invalid = typeMismatch "FirefoxOptions" invalid

instance ToJSON FirefoxOptions where
  toJSON FirefoxOptions{..} = object_
    [ "binary" .=? (toJSON <$> _firefox_binary)
    , "args" .=? (toJSON <$> _firefox_args)
    ]

instance Arbitrary FirefoxOptions where
  arbitrary = FirefoxOptions
    <$> arbitrary
    <*> arbitrary

-- | All members set to `Nothing`.
defaultFirefoxOptions :: FirefoxOptions
defaultFirefoxOptions = FirefoxOptions
  { _firefox_binary = Nothing
  , _firefox_args = Nothing
  }



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#proxy>.
data ProxyConfig = ProxyConfig
  { _proxy_type :: Maybe ProxyType -- ^ @proxyType@
  , _proxy_autoconfig_url :: Maybe String -- ^ @proxyAutoconfigUrl@
  , _ftp_proxy :: Maybe HostAndOptionalPort -- ^ @ftpProxy@
  , _http_proxy :: Maybe HostAndOptionalPort -- ^ @httpProxy@
  , _no_proxy :: Maybe [String] -- ^ @noProxy@
  , _ssl_proxy :: Maybe HostAndOptionalPort -- ^ @sslProxy@
  , _socks_proxy :: Maybe HostAndOptionalPort -- ^ @socksProxy@
  , _socks_version :: Maybe Int -- ^ @socksVersion@
  } deriving (Eq, Show)

instance FromJSON ProxyConfig where
  parseJSON (Object v) = ProxyConfig
    <$> v .:? "proxyType"
    <*> v .:? "proxyAutoconfigUrl"
    <*> v .:? "ftpProxy"
    <*> v .:? "httpProxy"
    <*> v .:? "noProxy"
    <*> v .:? "sslProxy"
    <*> v .:? "socksProxy"
    <*> v .:? "socksVersion"
  parseJSON invalid = typeMismatch "ProxyConfig" invalid

instance ToJSON ProxyConfig where
  toJSON ProxyConfig{..} = object_
    [ "proxyType" .=? (toJSON <$> _proxy_type)
    , "proxyAutoconfigUrl" .=? (toJSON <$> _proxy_autoconfig_url)
    , "ftpProxy" .=? (toJSON <$> _ftp_proxy)
    , "httpProxy" .=? (toJSON <$> _http_proxy)
    , "noProxy" .=? (toJSON <$> _no_proxy)
    , "sslProxy" .=? (toJSON <$> _ssl_proxy)
    , "socksProxy" .=? (toJSON <$> _socks_proxy)
    , "socksVersion" .=? (toJSON <$> _socks_version)
    ]

instance Arbitrary ProxyConfig where
  arbitrary = ProxyConfig
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

-- | `ProxyConfig` object with all members set to `Nothing`.
emptyProxyConfig :: ProxyConfig
emptyProxyConfig = ProxyConfig
  { _proxy_type = Nothing
  , _proxy_autoconfig_url = Nothing
  , _ftp_proxy = Nothing
  , _http_proxy = Nothing
  , _no_proxy = Nothing
  , _ssl_proxy = Nothing
  , _socks_proxy = Nothing
  , _socks_version = Nothing
  }



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-host-and-optional-port>.
data HostAndOptionalPort = HostAndOptionalPort
  { _url_host :: Host
  , _url_port :: Maybe Port
  } deriving (Eq, Show)

instance FromJSON HostAndOptionalPort where
  parseJSON (String x) =
    let string = unpack x in
    case span (/= ':') string of
      ("",_) -> malformedValue "Host" string
      (str,[]) -> case mkHost str of
        Nothing -> malformedValue "Host" string
        Just h -> return $ HostAndOptionalPort
          { _url_host = h
          , _url_port = Nothing
          }
      (str,":") -> malformedValue "Port" string
      (str,':':rest) -> case mkHost str of
        Nothing -> malformedValue "Host" string
        Just h -> case mkPort rest of
          Nothing -> malformedValue "Port" rest
          Just p -> return $ HostAndOptionalPort
            { _url_host = h
            , _url_port = Just p
            }
      (str,rest) -> malformedValue "Host" string
  parseJSON invalid = typeMismatch "HostAndOptionalPort" invalid

instance ToJSON HostAndOptionalPort where
  toJSON HostAndOptionalPort{..} = case _url_port of
    Nothing -> String $ pack $ show _url_host
    Just pt -> String $ pack $ concat [show _url_host, ":", show pt]

instance Arbitrary HostAndOptionalPort where
  arbitrary = HostAndOptionalPort
    <$> arbitrary
    <*> arbitrary



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-proxytype>.
data ProxyType
  = ProxyPac -- ^ @pac@
  | ProxyDirect -- ^ @direct@
  | ProxyAutodetect -- ^ @autodetect@
  | ProxySystem -- ^ @system@
  | ProxyManual -- ^ @manual@
  deriving (Eq, Show, Enum, Bounded)

instance FromJSON ProxyType where
  parseJSON (String x) = case x of
    "pac" -> return ProxyPac
    "direct" -> return ProxyDirect
    "autodetect" -> return ProxyAutodetect
    "system" -> return ProxySystem
    "manual" -> return ProxyManual
    _ -> unrecognizedValue "ProxyType" x
  parseJSON invalid = typeMismatch "ProxyType" invalid

instance ToJSON ProxyType where
  toJSON x = case x of
    ProxyPac -> String "pac"
    ProxyDirect -> String "direct"
    ProxyAutodetect -> String "autodetect"
    ProxySystem -> String "system"
    ProxyManual -> String "manual"

instance Arbitrary ProxyType where
  arbitrary = arbitraryBoundedEnum





-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-timeouts>.
data TimeoutConfig = TimeoutConfig
  { _script :: Maybe Int -- ^ @script@
  , _page_load :: Maybe Int -- ^ @pageLoad@
  , _implicit :: Maybe Int -- ^ @implicit@
  } deriving (Eq, Show)

instance FromJSON TimeoutConfig where
  parseJSON (Object v) = TimeoutConfig
    <$> v .:? "script"
    <*> v .:? "pageLoad"
    <*> v .:? "implicit"
  parseJSON invalid = typeMismatch "TimeoutConfig" invalid

instance ToJSON TimeoutConfig where
  toJSON TimeoutConfig{..} = object_
    [ "script" .== (toJSON <$> _script)
    , "pageLoad" .== (toJSON <$> _page_load)
    , "implicit" .== (toJSON <$> _implicit)
    ]

instance Arbitrary TimeoutConfig where
  arbitrary = TimeoutConfig
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

-- | `TimeoutConfig` object with all members set to `Nothing`.
emptyTimeoutConfig :: TimeoutConfig
emptyTimeoutConfig = TimeoutConfig
  { _script = Nothing
  , _page_load = Nothing
  , _implicit = Nothing
  }



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-table-of-location-strategies>.
data LocationStrategy
  = CssSelector -- ^ @css selector@
  | LinkTextSelector -- ^ @link text@
  | PartialLinkTextSelector -- ^ @partial link text@
  | TagName -- ^ @tag name@
  | XPathSelector -- ^ @xpath@
  deriving (Eq, Show, Enum, Bounded)

instance FromJSON LocationStrategy where
  parseJSON (String x) = case x of
    "css selector" -> return CssSelector
    "link text" -> return LinkTextSelector
    "partial link text" -> return PartialLinkTextSelector
    "tag name" -> return TagName
    "xpath" -> return XPathSelector
    _ -> unrecognizedValue "LocationStrategy" x
  parseJSON invalid = typeMismatch "LocationStrategy" invalid

instance ToJSON LocationStrategy where
  toJSON x = case x of
    CssSelector -> String "css selector"
    LinkTextSelector -> String "link text"
    PartialLinkTextSelector -> String "partial link text"
    TagName -> String "tag name"
    XPathSelector -> String "xpath"

instance Arbitrary LocationStrategy where
  arbitrary = arbitraryBoundedEnum



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-input-sources>.
data InputSource
  = NullInputSource -- ^ @null@
  | KeyInputSource -- ^ @key@
  | PointerInputSource -- ^ @pointer@
  deriving (Eq, Show, Enum, Bounded)

instance FromJSON InputSource where
  parseJSON (String x) = case x of
    "null" -> return NullInputSource
    "key" -> return KeyInputSource
    "pointer" -> return PointerInputSource
    _ -> unrecognizedValue "InputSource" x
  parseJSON invalid = typeMismatch "InputSource" invalid

instance ToJSON InputSource where
  toJSON x = case x of
    NullInputSource -> String "null"
    KeyInputSource -> String "key"
    PointerInputSource -> String "pointer"

instance Arbitrary InputSource where
  arbitrary = arbitraryBoundedEnum



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-pointer-input-state>.
data PointerSubtype
  = PointerMouse -- ^ @mouse@
  | PointerPen -- ^ @pen@
  | PointerTouch -- ^ @touch@
  deriving (Eq, Show, Enum, Bounded)

instance FromJSON PointerSubtype where
  parseJSON (String x) = case x of
    "mouse" -> return PointerMouse
    "pen" -> return PointerPen
    "touch" -> return PointerTouch
    _ -> unrecognizedValue "PointerSubtype" x
  parseJSON invalid = typeMismatch "PointerSubtype" invalid

instance ToJSON PointerSubtype where
  toJSON x = case x of 
    PointerMouse -> String "mouse"
    PointerPen -> String "pen"
    PointerTouch -> String "touch"

instance Arbitrary PointerSubtype where
  arbitrary = arbitraryBoundedEnum



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#processing-actions-requests>.
data Action = Action
  { _input_source_type :: Maybe InputSource -- ^ @type@
  , _input_source_id :: Maybe String -- ^ @id@
  , _input_source_parameters :: Maybe InputSourceParameter -- ^ @parameters@
  , _action_items :: [ActionItem] -- ^ @actions@
  } deriving (Eq, Show)

instance FromJSON Action where
  parseJSON (Object v) = Action
    <$> v .:? "type"
    <*> v .:? "id"
    <*> v .:? "parameters"
    <*> v .:  "actions"
  parseJSON invalid = typeMismatch "Action" invalid

instance ToJSON Action where
  toJSON Action{..} = object_
    [ "type" .=? (toJSON <$> _input_source_type)
    , "id" .=? (toJSON <$> _input_source_id)
    , "parameters" .=? (toJSON <$> _input_source_parameters)
    , "actions" .== (toJSON <$> _action_items)
    ]

instance Arbitrary Action where
  arbitrary = Action
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

-- | All members set to `Nothing` except `_action_items`, which is the empty list.
emptyAction = Action
  { _input_source_type = Nothing
  , _input_source_id = Nothing
  , _input_source_parameters = Nothing
  , _action_items = []
  }



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#terminology-0>.
data ActionType
  = PauseAction -- ^ @pause@
  | KeyUpAction -- ^ @keyUp@
  | KeyDownAction -- ^ @keyDown@
  | PointerDownAction -- ^ @pointerDown@
  | PointerUpAction -- ^ @pointerUp@
  | PointerMoveAction -- ^ @pointerMove@
  | PointerCancelAction -- ^ @pointerCancel@
  deriving (Eq, Show, Enum, Bounded)

instance FromJSON ActionType where
  parseJSON (String x) = case x of
    "pause" -> return PauseAction
    "keyUp" -> return KeyUpAction
    "keyDown" -> return KeyDownAction
    "pointerDown" -> return PointerDownAction
    "pointerUp" -> return PointerUpAction
    "pointerMove" -> return PointerMoveAction
    "pointerCancel" -> return PointerCancelAction
    _ -> unrecognizedValue "ActionType" x
  parseJSON invalid = typeMismatch "ActionType" invalid

instance ToJSON ActionType where
  toJSON x = case x of
    PauseAction -> String "pause"
    KeyUpAction -> String "keyUp"
    KeyDownAction -> String "keyDown"
    PointerDownAction -> String "pointerDown"
    PointerUpAction -> String "pointerUp"
    PointerMoveAction -> String "pointerMove"
    PointerCancelAction -> String "pointerCancel"

instance Arbitrary ActionType where
  arbitrary = arbitraryBoundedEnum



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-pointer-input-state>.
data InputSourceParameter = InputSourceParameter
  { _pointer_subtype :: Maybe PointerSubtype -- ^ @subtype@
  } deriving (Eq, Show)

instance FromJSON InputSourceParameter where
  parseJSON (Object v) = InputSourceParameter
    <$> v .:? "subtype"
  parseJSON invalid = typeMismatch "InputSourceParameter" invalid

instance ToJSON InputSourceParameter where
  toJSON InputSourceParameter{..} = object_
    [ "subtype" .=? (toJSON <$> _pointer_subtype)
    ]

instance Arbitrary InputSourceParameter where
  arbitrary = InputSourceParameter
    <$> arbitrary



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-process-an-input-source-action-sequence>.
data ActionItem = ActionItem
  { _action_type :: Maybe ActionType -- ^ @type@
  , _action_duration :: Maybe Int -- ^ @duration@
  , _action_origin :: Maybe String -- ^ @origin@
  , _action_value :: Maybe String -- ^ @value@
  , _action_button :: Maybe Int -- ^ @button@
  , _action_x :: Maybe Int -- ^ @x@
  , _action_y :: Maybe Int -- ^ @y@
  } deriving (Eq, Show)

instance FromJSON ActionItem where
  parseJSON (Object v) = ActionItem
    <$> v .:? "type"
    <*> v .:? "duration"
    <*> v .:? "origin"
    <*> v .:? "value"
    <*> v .:? "button"
    <*> v .:? "x"
    <*> v .:? "y"
  parseJSON invalid = typeMismatch "ActionItem" invalid

instance ToJSON ActionItem where
  toJSON ActionItem{..} = object_
    [ "type" .=? (toJSON <$> _action_type)
    , "duration" .=? (toJSON <$> _action_duration)
    , "origin" .=? (toJSON <$> _action_origin)
    , "value" .=? (toJSON <$> _action_value)
    , "button" .=? (toJSON <$> _action_button)
    , "x" .=? (toJSON <$> _action_x)
    , "y" .=? (toJSON <$> _action_y)
    ]

instance Arbitrary ActionItem where
  arbitrary = ActionItem
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

-- | All members set to `Nothing`.
emptyActionItem :: ActionItem
emptyActionItem = ActionItem
  { _action_type = Nothing
  , _action_duration = Nothing
  , _action_origin = Nothing
  , _action_value = Nothing
  , _action_button = Nothing
  , _action_x = Nothing
  , _action_y = Nothing
  }



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#get-element-rect>.
data Rect = Rect
  { _rect_x :: Scientific -- ^ @x@
  , _rect_y :: Scientific -- ^ @y@
  , _rect_width :: Scientific -- ^ @width@
  , _rect_height :: Scientific -- ^ @height@
  } deriving (Eq, Show)

instance ToJSON Rect where
  toJSON Rect{..} = object
    [ "x" .= (toJSON _rect_x)
    , "y" .= (toJSON _rect_y)
    , "width" .= (toJSON _rect_width)
    , "height" .= (toJSON _rect_height)
    ]

instance FromJSON Rect where
  parseJSON (Object v) = Rect
    <$> v .: "x"
    <*> v .: "y"
    <*> v .: "width"
    <*> v .: "height"
  parseJSON invalid = typeMismatch "Rect" invalid

arbScientific :: Gen Scientific
arbScientific = scientific <$> arbitrary <*> arbitrary
  

instance Arbitrary Rect where
  arbitrary = Rect
    <$> arbScientific
    <*> arbScientific
    <*> arbScientific
    <*> arbScientific

-- | All members set to `Nothing`.
emptyRect :: Rect
emptyRect = Rect
  { _rect_x = 0
  , _rect_y = 0
  , _rect_width = 0
  , _rect_height = 0
  }



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-known-prompt-handling-approaches-table>.
data PromptHandler
  = DismissPrompts -- ^ @dismiss@
  | AcceptPrompts -- ^ @accept@
  | DismissPromptsAndNotify -- ^ @dismiss and notify@
  | AcceptPromptsAndNotify -- ^ @accept and notify@
  | IgnorePrompts -- ^ @ignore@
  deriving (Eq, Show, Enum, Bounded)

instance FromJSON PromptHandler where
  parseJSON (String x) = case x of
    "dismiss" -> return DismissPrompts
    "accept" -> return AcceptPrompts
    "dismiss and notify" -> return DismissPromptsAndNotify
    "accept and notify" -> return AcceptPromptsAndNotify
    "ignore" -> return IgnorePrompts
    _ -> unrecognizedValue "PromptHandler" x
  parseJSON invalid = typeMismatch "PromptHandler" invalid

instance ToJSON PromptHandler where
  toJSON x = case x of
    DismissPrompts -> String "dismiss"
    AcceptPrompts -> String "accept"
    DismissPromptsAndNotify -> String "dismiss and notify"
    AcceptPromptsAndNotify -> String "accept and notify"
    IgnorePrompts -> String "ignore"

instance Arbitrary PromptHandler where
  arbitrary = arbitraryBoundedEnum



-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#dfn-table-for-cookie-conversion>.
data Cookie = Cookie
  { _cookie_name :: Maybe String -- ^ @name@
  , _cookie_value :: Maybe String -- ^ @value@
  , _cookie_path :: Maybe String -- ^ @path@
  , _cookie_domain :: Maybe String -- ^ @domain@
  , _cookie_secure :: Maybe Bool -- ^ @secure@
  , _cookie_http_only :: Maybe Bool -- ^ @httpOnly@
  , _cookie_expiry_time :: Maybe String -- ^ @expiryTime@
  } deriving (Eq, Show)

instance ToJSON Cookie where
  toJSON Cookie{..} = object_
    [ "name" .=? (toJSON <$> _cookie_name)
    , "value" .=? (toJSON <$> _cookie_value)
    , "path" .=? (toJSON <$> _cookie_path)
    , "domain" .=? (toJSON <$> _cookie_domain)
    , "secure" .=? (toJSON <$> _cookie_secure)
    , "httpOnly" .=? (toJSON <$> _cookie_http_only)
    , "expiryTime" .=? (toJSON <$> _cookie_expiry_time)
    ]

instance FromJSON Cookie where
  parseJSON (Object v) = Cookie
    <$> v .:? "name"
    <*> v .:? "value"
    <*> v .:? "path"
    <*> v .:? "domain"
    <*> v .:? "secure"
    <*> v .:? "httpOnly"
    <*> v .:? "expiryTime"
  parseJSON invalid = typeMismatch "Cookie" invalid

instance Arbitrary Cookie where
  arbitrary = Cookie
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

-- | All members set to `Nothing`.
emptyCookie :: Cookie
emptyCookie = Cookie
  { _cookie_name = Nothing
  , _cookie_value = Nothing
  , _cookie_path = Nothing
  , _cookie_domain = Nothing
  , _cookie_secure = Nothing
  , _cookie_http_only = Nothing
  , _cookie_expiry_time = Nothing
  }

-- | All members other than @name@ and @value@ set to `Nothing`.
cookie
  :: String -- ^ @name@
  -> String -- ^ @value@
  -> Cookie
cookie name value = emptyCookie
  { _cookie_name = Just name
  , _cookie_value = Just value
  }

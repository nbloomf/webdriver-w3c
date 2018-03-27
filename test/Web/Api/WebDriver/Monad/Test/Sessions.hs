{-# LANGUAGE BangPatterns #-}
module Web.Api.WebDriver.Monad.Test.Sessions (
    _exit_success_cases
  , _unknown_error_cases
  ) where

import Web.Api.Http
import Web.Api.WebDriver

_exit_success_cases
  :: (Effectful m)
  => [(String, WebDriver m ())]
_exit_success_cases =
  [ ( "sessionStatus"
    , do
        (!r,!m) <- sessionStatus
        return ()
    )

  , ( "getTimeouts"
    , do
        !timeouts <- getTimeouts
        return ()
    )

  , ( "setTimeouts"
    , do
        () <- setTimeouts emptyTimeoutConfig
        return ()
    )

  , ( "navigateTo (existing site)"
    , do
        () <- navigateTo "https://www.example.org"
        return ()
    )

  , ( "navigateToStealth"
    , do
        () <- navigateToStealth "https://www.example.org"
        return ()
    )

  , ( "getCurrentUrl"
    , do
        !url <- getCurrentUrl
        return ()
    )

  , ( "goBack"
    , do
        () <- goBack
        return ()
    )

  , ( "goForward"
    , do
        () <- goForward
        return ()
    )

  , ( "pageRefresh"
    , do
        () <- pageRefresh
        return ()
    )

  , ( "getTitle"
    , do
        !title <- getTitle
        return ()
    )

  , ( "getWindowHandle"
    , do
        !handle <- getWindowHandle
        return ()
    )

  -- TODO: closeWindow

  , ( "switchToWindow"
    , do
        hs <- getWindowHandles
        case hs of
          [] -> error "no window handles"
          h:_ -> do
            () <- switchToWindow h
            return ()
    )

  , ( "getWindowHandles"
    , do
        navigateTo "https://www.w3.org"
        !handles <- getWindowHandles
        case handles of
          [] -> return ()
          (!x):xs -> return ()
    )

  , ( "switchToFrame"
    , do
        navigateTo "https://www.w3.org"
        () <- switchToFrame TopLevelFrame
        return ()
    )

  -- TODO: switchToParentFrame

  , ( "getWindowRect"
    , do
        !rect <- getWindowRect
        return ()
    )

  -- TODO: setWindowRect

  , ( "maximizeWindow"
    , do
        !rect <- maximizeWindow
        return ()
    )

  , ( "minimizeWindow"
    , do
        !rect <- minimizeWindow
        return ()
    )

  , ( "fullscreenWindow"
    , do
        !rect <- fullscreenWindow
        return ()
    )

  , ( "findElement"
    , do
        navigateTo "https://www.w3.org"
        !element <- findElement CssSelector "body"
        !element <- findElement LinkTextSelector "Standards"
        !element <- findElement PartialLinkTextSelector "Standards"
        !element <- findElement TagName "body"
        !element <- findElement XPathSelector "*"
        return ()
    )

  , ( "findElements"
    , do
        navigateTo "https://www.w3.org"
        !elements <- findElements CssSelector "body"
        case elements of
          [] -> return ()
          (!x):xs -> return ()
        !elements <- findElements LinkTextSelector "Standards"
        case elements of
          [] -> return ()
          (!x):xs -> return ()
        !elements <- findElements PartialLinkTextSelector "Standards"
        case elements of
          [] -> return ()
          (!x):xs -> return ()
        !elements <- findElements TagName "body"
        case elements of
          [] -> return ()
          (!x):xs -> return ()
        !elements <- findElements XPathSelector "*"
        case elements of
          [] -> return ()
          (!x):xs -> return ()
    )

  , ( "findElementFromElement"
    , do
        navigateTo "https://www.w3.org"
        root <- findElement CssSelector "body"
        !element <- findElementFromElement CssSelector "p" root
        !element <- findElementFromElement LinkTextSelector "Standards" root
        !element <- findElementFromElement PartialLinkTextSelector "Standards" root
        !element <- findElementFromElement TagName "p" root
        !element <- findElementFromElement XPathSelector "*" root
        return ()
    )

  , ( "findElementsFromElement"
    , do
        navigateTo "https://www.w3.org"
        root <- findElement CssSelector "body"
        !elements <- findElementsFromElement CssSelector "p" root
        case elements of
          [] -> return ()
          (!x):xs -> return ()
        !elements <- findElementsFromElement LinkTextSelector "Standards" root
        case elements of
          [] -> return ()
          (!x):xs -> return ()
        !elements <- findElementsFromElement PartialLinkTextSelector "Standards" root
        case elements of
          [] -> return ()
          (!x):xs -> return ()
        !elements <- findElementsFromElement TagName "p" root
        case elements of
          [] -> return ()
          (!x):xs -> return ()
        !elements <- findElementsFromElement XPathSelector "*" root
        case elements of
          [] -> return ()
          (!x):xs -> return ()
    )

  , ( "getActiveElement"
    , do
        !element <- getActiveElement
        return ()
    )

  , ( "isElementSelected"
    , do
        navigateTo "https://www.w3.org"
        !element <- getActiveElement
        !p <- isElementSelected element
        return ()
    )

  , ( "getElementAttribute"
    , do
        navigateTo "https://www.w3.org"
        !element <- getActiveElement
        !attr <- getElementAttribute element "href"
        return ()
    )

  -- TODO: getElementProperty

  -- TODO: getElementCssValue

  , ( "getElementText"
    , do
        navigateTo "https://www.w3.org"
        !element <- getActiveElement
        !text <- getElementText element
        return ()
    )

  -- TODO: getElementTagName

  , ( "getElementRect"
    , do
        navigateTo "https://www.w3.org"
        !element <- getActiveElement
        !rect <- getElementRect element
        return ()
    )

  , ( "isElementEnabled"
    , do
        navigateTo "https://www.w3.org"
        !element <- getActiveElement
        !p <- isElementEnabled element
        return ()
    )

  , ( "elementClick"
    , do
        navigateTo "https://www.w3.org"
        !root <- findElement CssSelector "body"
        () <- elementClick root
        return ()
    )

  , ( "elementClear"
    , do
        navigateTo "https://www.w3.org"
        !element <- findElement CssSelector "input.text"
        !rect <- elementClear element
        return ()
    )

  , ( "elementSendKeys"
    , do
        navigateTo "https://www.w3.org"
        !element <- findElement CssSelector "input.text"
        !rect <- elementSendKeys element "foo"
        return ()
    )

  , ( "getPageSource"
    , do
        navigateTo "https://www.w3.org"
        !src <- getPageSource
        return ()
    )

  -- TODO: executeScript

  -- TODO: executeAsyncScript

  -- TODO: getAllCookies

  -- TODO: getNamedCookie

  -- TODO: addCookie

  -- TODO: deleteCookie

  , ( "deleteAllCookies"
    , do
        () <- deleteAllCookies
        return ()
    )

  , ( "performActions (keyboard)"
    , do
        () <- performActions [ press UnidentifiedKey ]
        () <- performActions [ press CancelKey ]
        () <- performActions [ press HelpKey ]
        () <- performActions [ press BackspaceKey ]
        () <- performActions [ press TabKey ]
        () <- performActions [ press ClearKey ]
        () <- performActions [ press ReturnKey ]
        () <- performActions [ press EnterKey ]
        () <- performActions [ press ShiftKey ]
        () <- performActions [ press ControlKey ]
        () <- performActions [ press AltKey ]
        () <- performActions [ press PauseKey ]
        () <- performActions [ press EscapeKey ]
        () <- performActions [ press PageUpKey ]
        () <- performActions [ press PageDownKey ]
        () <- performActions [ press EndKey ]
        () <- performActions [ press HomeKey ]
        () <- performActions [ press ArrowLeftKey ]
        () <- performActions [ press ArrowUpKey ]
        () <- performActions [ press ArrowRightKey ]
        () <- performActions [ press ArrowDownKey ]
        () <- performActions [ press InsertKey ]
        () <- performActions [ press DeleteKey ]
        () <- performActions [ press F1Key ]
        () <- performActions [ press F2Key ]
        () <- performActions [ press F3Key ]
        () <- performActions [ press F4Key ]
        () <- performActions [ press F5Key ]
        () <- performActions [ press F6Key ]
        () <- performActions [ press F7Key ]
        () <- performActions [ press F8Key ]
        () <- performActions [ press F9Key ]
        () <- performActions [ press F10Key ]
        () <- performActions [ press F11Key ]
        () <- performActions [ press F12Key ]
        () <- performActions [ press MetaKey ]
        () <- performActions [ press ZenkakuHankakuKey ]
        return ()
    )

  , ( "performStealthActions (keyboard)"
    , do
        () <- performStealthActions [ press EnterKey ]
        return ()
    )

  -- TODO: releaseActions

  -- TODO: dismissAlert

  -- TODO: acceptAlert

  -- TODO: getAlertText

  -- TODO: sendAlertText

  -- TODO: takeScreenshot

  -- TODO: takeElementScreenshot
  ]

_unknown_error_cases
  :: (Effectful m)
  => [(String, WebDriver m ())]
_unknown_error_cases =
  [ ( "navigateTo (nonexistent site)"
    , do
        () <- navigateTo "https://fake.example"
        return ()
    )
  ]

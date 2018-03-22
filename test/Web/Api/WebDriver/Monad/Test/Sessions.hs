{-# LANGUAGE BangPatterns #-}
module Web.Api.WebDriver.Monad.Test.Sessions (
    _exit_success_cases
  ) where

import Web.Api.Http
import Web.Api.WebDriver

_exit_success_cases
  :: (Effectful m)
  => [(String, WebDriver m ())]
_exit_success_cases =
  [ ( "goBack"
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

  , ( "navigateTo"
    , do
        () <- navigateTo "https://www.w3.org"
        return ()
    )

  , ( "navigateToStealth"
    , do
        () <- navigateToStealth "https://www.w3.org"
        return ()
    )

  , ( "getCurrentUrl"
    , do
        !url <- getCurrentUrl
        return ()
    )

  , ( "getTitle"
    , do
        !title <- getTitle
        return ()
    )

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

  , ( "getTimeouts"
    , do
        !timeouts <- getTimeouts
        return ()
    )

  , ( "getWindowRect"
    , do
        !rect <- getWindowRect
        return ()
    )

  , ( "deleteAllCookies"
    , do
        () <- deleteAllCookies
        return ()
    )

  , ( "findElement (CSS Selector)"
    , do
        navigateTo "https://www.w3.org"
        !element <- findElement CssSelector "body"
        return ()
    )

  , ( "findElement (Link Text)"
    , do
        navigateTo "https://www.w3.org"
        !element <- findElement LinkTextSelector "Standards"
        return ()
    )

  , ( "findElement (Partial Link Text)"
    , do
        navigateTo "https://www.w3.org"
        !element <- findElement PartialLinkTextSelector "Standards"
        return ()
    )

  , ( "findElement (Tag Name)"
    , do
        navigateTo "https://www.w3.org"
        !element <- findElement TagName "body"
        return ()
    )

  , ( "findElement (XPath)"
    , do
        navigateTo "https://www.w3.org"
        !element <- findElement XPathSelector "*"
        return ()
    )

  , ( "findElements (CSS Selector)"
    , do
        navigateTo "https://www.w3.org"
        !elements <- findElements CssSelector "body"
        case elements of
          [] -> return ()
          (!x):xs -> return ()
    )

  , ( "findElements (Link Text)"
    , do
        navigateTo "https://www.w3.org"
        !elements <- findElements LinkTextSelector "Standards"
        case elements of
          [] -> return ()
          (!x):xs -> return ()
    )

  , ( "findElements (Partial Link Text)"
    , do
        navigateTo "https://www.w3.org"
        !elements <- findElements PartialLinkTextSelector "Standards"
        case elements of
          [] -> return ()
          (!x):xs -> return ()
    )

  , ( "findElements (Tag Name)"
    , do
        navigateTo "https://www.w3.org"
        !elements <- findElements TagName "body"
        case elements of
          [] -> return ()
          (!x):xs -> return ()
    )

  , ( "findElements (XPath)"
    , do
        navigateTo "https://www.w3.org"
        !elements <- findElements XPathSelector "*"
        case elements of
          [] -> return ()
          (!x):xs -> return ()
    )

  , ( "findElementFromElement (CSS Selector)"
    , do
        navigateTo "https://www.w3.org"
        root <- findElement CssSelector "body"
        !element <- findElementFromElement CssSelector "p" root
        return ()
    )

  , ( "findElementFromElement (Link Text)"
    , do
        navigateTo "https://www.w3.org"
        root <- findElement CssSelector "body"
        !element <- findElementFromElement LinkTextSelector "Standards" root
        return ()
    )

  , ( "findElementFromElement (Partial Link Text)"
    , do
        navigateTo "https://www.w3.org"
        root <- findElement CssSelector "body"
        !element <- findElementFromElement PartialLinkTextSelector "Standards" root
        return ()
    )

  , ( "findElementFromElement (Tag Name)"
    , do
        navigateTo "https://www.w3.org"
        root <- findElement CssSelector "body"
        !element <- findElementFromElement TagName "p" root
        return ()
    )

  , ( "findElementFromElement (XPath)"
    , do
        navigateTo "https://www.w3.org"
        root <- findElement CssSelector "body"
        !element <- findElementFromElement XPathSelector "*" root
        return ()
    )

  , ( "findElementsFromElement (CSS Selector)"
    , do
        navigateTo "https://www.w3.org"
        root <- findElement CssSelector "body"
        !elements <- findElementsFromElement CssSelector "p" root
        case elements of
          [] -> return ()
          (!x):xs -> return ()
    )

  , ( "findElementsFromElement (Link Text)"
    , do
        navigateTo "https://www.w3.org"
        root <- findElement CssSelector "body"
        !elements <- findElementsFromElement LinkTextSelector "Standards" root
        case elements of
          [] -> return ()
          (!x):xs -> return ()
    )

  , ( "findElementsFromElement (Partial Link Text)"
    , do
        navigateTo "https://www.w3.org"
        root <- findElement CssSelector "body"
        !elements <- findElementsFromElement PartialLinkTextSelector "Standards" root
        case elements of
          [] -> return ()
          (!x):xs -> return ()
    )

  , ( "findElementsFromElement (Tag Name)"
    , do
        navigateTo "https://www.w3.org"
        root <- findElement CssSelector "body"
        !elements <- findElementsFromElement TagName "p" root
        case elements of
          [] -> return ()
          (!x):xs -> return ()
    )

  , ( "findElementsFromElement (XPath)"
    , do
        navigateTo "https://www.w3.org"
        root <- findElement CssSelector "body"
        !elements <- findElementsFromElement XPathSelector "*" root
        case elements of
          [] -> return ()
          (!x):xs -> return ()
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

  , ( "getWindowHandle"
    , do
        !handle <- getWindowHandle
        return ()
    )

  , ( "closeWindow"
    , do
        !handles <- getWindowHandle
        case handles of
          [] -> return ()
          (!x:xs) -> return ()
    )
  ]

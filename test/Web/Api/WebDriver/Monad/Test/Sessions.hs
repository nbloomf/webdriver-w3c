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
  ]

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
  ]

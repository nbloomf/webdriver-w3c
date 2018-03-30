{-# LANGUAGE BangPatterns #-}
module Web.Api.WebDriver.Monad.Test.Session.Success (
    successfulExit
  ) where

import Data.Typeable (Typeable)

import Web.Api.Http
import Web.Api.WebDriver
import Test.Tasty.WebDriver

import qualified Test.Tasty as T


unexpectedError
  :: (Effectful m, Typeable m)
  => Err WebDriverError
  -> WebDriver m ()
unexpectedError _ = assertFailure "unexpected error"


successfulExit :: (Effectful m, Typeable m) => m () -> T.TestTree
successfulExit x = T.testGroup "Successful Exit"
  [ testCase "sessionStatus" (_test_sessionStatus_success x)
  , testCase "getTimeouts" (_test_getTimeouts_success x)
  , testCase "setTimeouts" (_test_setTimeouts_success x)
  , testCase "navigateTo" (_test_navigateTo_success x)
  , testCase "navigateToStealth" (_test_navigateToStealth_success x)
  , testCase "getCurrentUrl" (_test_getCurrentUrl_success x)
  , testCase "goBack" (_test_goBack_success x)
  , testCase "goForward" (_test_goForward_success x)
  , testCase "pageRefresh" (_test_pageRefresh_success x)
  , testCase "getTitle" (_test_getTitle_success x)
  , testCase "getWindowHandle" (_test_getWindowHandle_success x)
  , testCase "switchToWindow" (_test_switchToWindow_success x)
  , testCase "getWindowHandles" (_test_getWindowHandles_success x)
  , testCase "switchToFrame" (_test_switchToFrame_success x)
  , testCase "getWindowRect" (_test_getWindowRect_success x)
  , testCase "maximizeWindow" (_test_maximizeWindow_success x)
  , testCase "minimizeWindow" (_test_minimizeWindow_success x)
  , testCase "fullscreenWindow" (_test_fullscreenWindow_success x)
  , testCase "findElement" (_test_findElement_success x)
  , testCase "findElements" (_test_findElements_success x)
  , testCase "findElementFromElement" (_test_findElementFromElement_success x)
  , testCase "findElementsFromElement" (_test_findElementsFromElement_success x)
  , testCase "getActiveElement" (_test_getActiveElement_success x)
  , testCase "isElementSelected" (_test_isElementSelected_success x)
  , testCase "getElementAttribute" (_test_getElementAttribute_success x)
  , testCase "getElementText" (_test_getElementText_success x)
  , testCase "getElementRect" (_test_getElementRect_success x)
  , testCase "isElementEnabled" (_test_isElementEnabled_success x)
  , testCase "elementClick" (_test_elementClick_success x)
  , testCase "elementClear" (_test_elementClear_success x)
  , testCase "elementSendKeys" (_test_elementSendKeys_success x)
  , testCase "getPageSource" (_test_getPageSource_success x)
  , testCase "getPageSourceStealth" (_test_getPageSourceStealth_success x)
  , testCase "deleteAllCookies" (_test_deleteAllCookies_success x)
  , testCase "performActions (keyboard)" (_test_performActions_keyboard_success x)
  , testCase "performStealthActions (keyboard)" (_test_performStealthActions_keyboard_success x)
  ]



_test_sessionStatus_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_sessionStatus_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      (!r,!m) <- sessionStatus
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getTimeouts_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getTimeouts_success _ =
  let
    session = do
      !timeouts <- getTimeouts
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_setTimeouts_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_setTimeouts_success _ =
  let
    session = do
      () <- setTimeouts emptyTimeoutConfig
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_navigateTo_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_navigateTo_success _ =
  let
    session = do
      () <- navigateTo "https://www.example.org"
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_navigateToStealth_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_navigateToStealth_success _ =
  let
    session = do
      () <- navigateToStealth "https://www.example.org"
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getCurrentUrl_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getCurrentUrl_success _ =
  let
    session = do
      !url <- getCurrentUrl
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_goBack_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_goBack_success _ =
  let
    session = do
      () <- goBack
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_goForward_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_goForward_success _ =
  let
    session = do
      () <- goForward
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_pageRefresh_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_pageRefresh_success _ =
  let
    session = do
      () <- pageRefresh
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getTitle_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getTitle_success _ =
  let
    session = do
      !title <- getTitle
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getWindowHandle_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getWindowHandle_success _ =
  let
    session = do
      !handle <- getWindowHandle
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



-- TODO: closeWindow



_test_switchToWindow_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_switchToWindow_success _ =
  let
    session = do
      hs <- getWindowHandles
      case hs of
        [] -> assertFailure "no window handles"
        (!h):_ -> do
          () <- switchToWindow h
          assertSuccess "yay"
          return ()

  in catchError session unexpectedError



_test_getWindowHandles_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getWindowHandles_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      !handles <- getWindowHandles
      case handles of
        [] -> do
          assertSuccess "yay"
          return ()
        (!x):xs -> do
          assertSuccess "yay"
          return ()

  in catchError session unexpectedError



_test_switchToFrame_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_switchToFrame_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      () <- switchToFrame TopLevelFrame
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



-- TODO: switchToParentFrame



_test_getWindowRect_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getWindowRect_success _ =
  let
    session = do
      !rect <- getWindowRect
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



-- TODO: setWindowRect



_test_maximizeWindow_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_maximizeWindow_success _ =
  let
    session = do
      !rect <- maximizeWindow
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_minimizeWindow_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_minimizeWindow_success _ =
  let
    session = do
      !rect <- minimizeWindow
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_fullscreenWindow_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_fullscreenWindow_success _ =
  let
    session = do
      !rect <- fullscreenWindow
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_findElement_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_findElement_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      !element <- findElement CssSelector "body"
      !element <- findElement LinkTextSelector "Standards"
      !element <- findElement PartialLinkTextSelector "Standards"
      !element <- findElement TagName "body"
      !element <- findElement XPathSelector "*"
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_findElements_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_findElements_success _ =
  let
    session = do
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
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_findElementFromElement_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_findElementFromElement_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      root <- findElement CssSelector "body"
      !element <- findElementFromElement CssSelector "p" root
      !element <- findElementFromElement LinkTextSelector "Standards" root
      !element <- findElementFromElement PartialLinkTextSelector "Standards" root
      !element <- findElementFromElement TagName "p" root
      !element <- findElementFromElement XPathSelector "*" root
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_findElementsFromElement_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_findElementsFromElement_success _ =
  let
    session = do
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
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getActiveElement_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getActiveElement_success _ =
  let
    session = do
      !element <- getActiveElement
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_isElementSelected_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_isElementSelected_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      !element <- getActiveElement
      !p <- isElementSelected element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getElementAttribute_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getElementAttribute_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      !element <- getActiveElement
      !attr <- getElementAttribute element "href"
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



-- TODO: getElementProperty



-- TODO: getElementCssValue



_test_getElementText_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getElementText_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      !element <- getActiveElement
      !text <- getElementText element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



-- TODO: getElementTagName



_test_getElementRect_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getElementRect_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      !element <- getActiveElement
      !rect <- getElementRect element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_isElementEnabled_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_isElementEnabled_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      !element <- getActiveElement
      !p <- isElementEnabled element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_elementClick_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_elementClick_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      !root <- findElement CssSelector "body"
      () <- elementClick root
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_elementClear_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_elementClear_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      !element <- findElement CssSelector "input.text"
      () <- elementClear element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_elementSendKeys_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_elementSendKeys_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      !element <- findElement CssSelector "input.text"
      () <- elementSendKeys element "foo"
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getPageSource_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getPageSource_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      !src <- getPageSource
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getPageSourceStealth_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getPageSourceStealth_success _ =
  let
    session = do
      navigateTo "https://www.w3.org"
      !src <- getPageSourceStealth
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



-- TODO: executeScript



-- TODO: executeAsyncScript



-- TODO: getAllCookies



-- TODO: getNamedCookie



-- TODO: addCookie



-- TODO: deleteCookie



_test_deleteAllCookies_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_deleteAllCookies_success _ =
  let
    session = do
      () <- deleteAllCookies
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_performActions_keyboard_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_performActions_keyboard_success _ =
  let
    session = do
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
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_performStealthActions_keyboard_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_performStealthActions_keyboard_success _ =
  let
    session = do
      () <- performStealthActions [ press EnterKey ]
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



-- TODO: releaseActions



-- TODO: dismissAlert



-- TODO: acceptAlert



-- TODO: getAlertText



-- TODO: sendAlertText



-- TODO: takeScreenshot



-- TODO: takeElementScreenshot

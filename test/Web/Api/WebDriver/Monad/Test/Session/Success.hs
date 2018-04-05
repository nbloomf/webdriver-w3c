{-# LANGUAGE BangPatterns #-}
module Web.Api.WebDriver.Monad.Test.Session.Success (
    successfulExit
  ) where

import Data.Typeable (Typeable)
import System.IO

import Web.Api.Http
import Web.Api.WebDriver
import Test.Tasty.WebDriver

import qualified Test.Tasty as T (TestTree, testGroup)


unexpectedError
  :: (Effectful m, Typeable m)
  => Err WebDriverError
  -> WebDriver m ()
unexpectedError e = assertFailure $ "Unexpected error:\n" ++ show e


successfulExit :: (Effectful m, Typeable m) => FilePath -> m () -> T.TestTree
successfulExit dir x =
  let path = dir ++ "/success.html" in
  T.testGroup "Successful Exit"
    [ testCase "sessionStatus" (_test_sessionStatus_success path x)
    , testCase "getTimeouts" (_test_getTimeouts_success x)
    , testCase "setTimeouts" (_test_setTimeouts_success x)
    , testCase "navigateTo" (_test_navigateTo_success path x)
    , testCase "navigateToStealth" (_test_navigateToStealth_success path x)
    , testCase "getCurrentUrl" (_test_getCurrentUrl_success x)
    , testCase "goBack" (_test_goBack_success x)
    , testCase "goForward" (_test_goForward_success x)
    , testCase "pageRefresh" (_test_pageRefresh_success x)
    , testCase "getTitle" (_test_getTitle_success x)
    , testCase "getWindowHandle" (_test_getWindowHandle_success x)
    , testCase "switchToWindow" (_test_switchToWindow_success x)
    , testCase "getWindowHandles" (_test_getWindowHandles_success path x)
    , testCase "switchToFrame" (_test_switchToFrame_success path x)
    , testCase "switchToParentFrame" (_test_switchToParentFrame_success path x)
    , testCase "getWindowRect" (_test_getWindowRect_success x)
    , testCase "setWindowRect" (_test_setWindowRect_success x)
    , testCase "maximizeWindow" (_test_maximizeWindow_success x)
    , testCase "minimizeWindow" (_test_minimizeWindow_success x)
    , testCase "fullscreenWindow" (_test_fullscreenWindow_success x)
    , testCase "findElement" (_test_findElement_success path x)
    , testCase "findElements" (_test_findElements_success path x)
    , testCase "findElementFromElement" (_test_findElementFromElement_success path x)
    , testCase "findElementsFromElement" (_test_findElementsFromElement_success path x)
    , testCase "getActiveElement" (_test_getActiveElement_success x)
    , testCase "isElementSelected" (_test_isElementSelected_success path x)
    , testCase "getElementAttribute" (_test_getElementAttribute_success path x)
    , testCase "getElementCssValue" (_test_getElementCssValue_success path x)
    , testCase "getElementText" (_test_getElementText_success path x)
    , testCase "getElementTagName" (_test_getElementTagName_success path x)
    , testCase "getElementRect" (_test_getElementRect_success path x)
    , testCase "isElementEnabled" (_test_isElementEnabled_success path x)
    , testCase "elementClick" (_test_elementClick_success path x)
    , testCase "elementClear" (_test_elementClear_success path x)
    , testCase "elementSendKeys" (_test_elementSendKeys_success path x)
    , testCase "getPageSource" (_test_getPageSource_success path x)
    , testCase "getPageSourceStealth" (_test_getPageSourceStealth_success path x)
    , testCase "getAllCookies" (_test_getAllCookies_success path x)
    , testCase "getNamedCookie" (_test_getNamedCookie_success path x)
    , testCase "deleteCookie" (_test_deleteCookie_success path x)
    , testCase "deleteAllCookies" (_test_deleteAllCookies_success x)
    , testCase "performActions (keyboard)" (_test_performActions_keyboard_success x)
    , testCase "performActionsStealth (keyboard)" (_test_performActionsStealth_keyboard_success x)
    , testCase "releaseActions" (_test_releaseActions_success x)
    , testCase "dismissAlert" (_test_dismissAlert_success path x)
    , testCase "acceptAlert" (_test_acceptAlert_success path x)
    , testCase "getAlertText" (_test_getAlertText_success path x)
    , testCase "sendAlertText" (_test_sendAlertText_success path x)
    , testCase "takeScreenshot" (_test_takeScreenshot_success path x)
    , testCase "takeElementScreenshot" (_test_takeElementScreenshot_success path x)
    ]



_test_sessionStatus_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_sessionStatus_success page _ =
  let
    session = do
      navigateTo page
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
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_navigateTo_success page _ =
  let
    session = do
      () <- navigateTo page
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_navigateToStealth_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_navigateToStealth_success page _ =
  let
    session = do
      () <- navigateToStealth page
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
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_getWindowHandles_success page _ =
  let
    session = do
      navigateTo page
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
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_switchToFrame_success page _ =
  let
    session = do
      navigateTo page
      () <- switchToFrame TopLevelFrame
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_switchToParentFrame_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_switchToParentFrame_success page _ =
  let
    session = do
      navigateTo page
      () <- switchToParentFrame
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getWindowRect_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_getWindowRect_success _ =
  let
    session = do
      !rect <- getWindowRect
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_setWindowRect_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_setWindowRect_success _ =
  let
    session = do
      !rect <- setWindowRect $ Rect
        { _rect_x = 0
        , _rect_y = 0
        , _rect_width = 640
        , _rect_height = 480
        }
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



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
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_findElement_success page _ =
  let
    session = do
      navigateTo page
      !element <- findElement CssSelector "body"
      !element <- findElement LinkTextSelector "A Link"
      !element <- findElement PartialLinkTextSelector "Link"
      !element <- findElement TagName "body"
      !element <- findElement XPathSelector "*"
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_findElements_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_findElements_success page _ =
  let
    session = do
      navigateTo page
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
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_findElementFromElement_success page _ =
  let
    session = do
      navigateTo page
      root <- findElement CssSelector "body"
      !element <- findElementFromElement CssSelector "p" root
      !element <- findElementFromElement LinkTextSelector "A Link" root
      !element <- findElementFromElement PartialLinkTextSelector "Link" root
      !element <- findElementFromElement TagName "p" root
      !element <- findElementFromElement XPathSelector "*" root
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_findElementsFromElement_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_findElementsFromElement_success page _ =
  let
    session = do
      navigateTo page
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
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_isElementSelected_success page _ =
  let
    session = do
      navigateTo page
      !element <- getActiveElement
      !p <- isElementSelected element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getElementAttribute_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_getElementAttribute_success page _ =
  let
    session = do
      navigateTo page
      !element <- getActiveElement
      !attr <- getElementAttribute element "href"
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



-- TODO: getElementProperty



_test_getElementCssValue_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_getElementCssValue_success page _ =
  let
    session = do
      navigateTo page
      !element <- findElement CssSelector "#super-cool"
      !text <- getElementCssValue element "text-decoration"
      case text of
        "none" -> assertSuccess "yay"
        _ -> assertFailure $ "expected 'none', got '" ++ text ++ "'"
      return ()

  in catchError session unexpectedError



_test_getElementText_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_getElementText_success page _ =
  let
    session = do
      navigateTo page
      !element <- findElement CssSelector ".test"
      !text <- getElementTagName element
      case text of
        "div" -> assertSuccess "yay"
        _ -> assertFailure $ "expected 'div', got '" ++ text ++ "'"
      return ()

  in catchError session unexpectedError



_test_getElementTagName_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_getElementTagName_success page _ =
  let
    session = do
      navigateTo page
      !element <- getActiveElement
      !text <- getElementText element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getElementRect_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_getElementRect_success page _ =
  let
    session = do
      navigateTo page
      !element <- getActiveElement
      !rect <- getElementRect element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_isElementEnabled_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_isElementEnabled_success page _ =
  let
    session = do
      navigateTo page
      !element <- getActiveElement
      !p <- isElementEnabled element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_elementClick_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_elementClick_success page _ =
  let
    session = do
      navigateTo page
      !root <- findElement CssSelector "body"
      () <- elementClick root
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_elementClear_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_elementClear_success page _ =
  let
    session = do
      navigateTo page
      !element <- findElement CssSelector "input[name=\"sometext\"]"
      () <- elementClear element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_elementSendKeys_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_elementSendKeys_success page _ =
  let
    session = do
      navigateTo page
      !element <- findElement CssSelector "input[name=\"sometext\"]"
      () <- elementSendKeys element "foo"
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getPageSource_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_getPageSource_success page _ =
  let
    session = do
      navigateTo page
      !src <- getPageSource
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getPageSourceStealth_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_getPageSourceStealth_success page _ =
  let
    session = do
      navigateTo page
      !src <- getPageSourceStealth
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



-- TODO: executeScript



-- TODO: executeAsyncScript



_test_getAllCookies_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_getAllCookies_success page _ =
  let
    session = do
      navigateTo page
      !jar <- getAllCookies
      case jar of
        [] -> assertSuccess "yay"
        (!x):_ -> assertFailure "unexpected cookie"
      return ()

  in catchError session unexpectedError



_test_getNamedCookie_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_getNamedCookie_success page _ =
  let
    session = do
      navigateTo page
      findElement CssSelector "#add-cookie-button" >>= elementClick
      !cookie <- getNamedCookie "fakeCookie"
      assertEqual (_cookie_name cookie) (Just "fakeCookie") "cookie name"
      assertEqual (_cookie_value cookie) (Just "fakeValue") "cookie name"
      return ()

  in catchError session unexpectedError



{- TODO: addCookie -}
{- note: file:// addresses do not like cookies -}



_test_deleteCookie_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_deleteCookie_success page _ =
  let
    session = do
      navigateTo page
      findElement CssSelector "#add-cookie-button" >>= elementClick
      () <- deleteCookie "fakeCookie"
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



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



_test_performActionsStealth_keyboard_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_performActionsStealth_keyboard_success _ =
  let
    session = do
      () <- performActionsStealth [ press EnterKey ]
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_releaseActions_success
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_releaseActions_success _ =
  let
    session = do
      () <- releaseActions
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_dismissAlert_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_dismissAlert_success page _ =
  let
    session = do
      navigateTo page
      findElement CssSelector "#alert-button" >>= elementClick
      () <- dismissAlert
      assertSuccess "yay alert"
      findElement CssSelector "#confirm-button" >>= elementClick
      () <- dismissAlert
      assertSuccess "yay confirm"
      findElement CssSelector "#prompt-button" >>= elementClick
      () <- dismissAlert
      assertSuccess "yay prompt"
      return ()

  in catchError session unexpectedError



_test_acceptAlert_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_acceptAlert_success page _ =
  let
    session = do
      navigateTo page
      findElement CssSelector "#alert-button" >>= elementClick
      () <- acceptAlert
      assertSuccess "yay alert"
      findElement CssSelector "#confirm-button" >>= elementClick
      () <- acceptAlert
      assertSuccess "yay confirm"
      findElement CssSelector "#prompt-button" >>= elementClick
      () <- acceptAlert
      assertSuccess "yay prompt"
      return ()

  in catchError session unexpectedError



_test_getAlertText_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_getAlertText_success page _ =
  let
    session = do
      navigateTo page
      findElement CssSelector "#alert-button" >>= elementClick
      !box <- getAlertText
      case box of
        Nothing -> assertFailure "oh no alert"
        Just msg -> assertEqual msg "WOO!!" "alert text"
      acceptAlert
      findElement CssSelector "#confirm-button" >>= elementClick
      !box <- getAlertText
      case box of
        Nothing -> assertFailure "oh no confirm"
        Just msg -> assertEqual msg "WOO!!" "confirm text"
      acceptAlert
      findElement CssSelector "#prompt-button" >>= elementClick
      !box <- getAlertText
      case box of
        Nothing -> assertFailure "oh no prompt"
        Just msg -> assertEqual msg "WOO!!" "prompt text"
      acceptAlert
      return ()

  in catchError session unexpectedError



_test_sendAlertText_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_sendAlertText_success page _ =
  let
    session = do
      navigateTo page
      findElement CssSelector "#prompt-button" >>= elementClick
      () <- sendAlertText "wut"
      assertSuccess "yay prompt"
      return ()

  in catchError session unexpectedError



_test_takeScreenshot_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_takeScreenshot_success page _ =
  let
    session = do
      navigateTo page
      !screenshot <- takeScreenshot
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_takeElementScreenshot_success
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_takeElementScreenshot_success page _ =
  let
    session = do
      navigateTo page
      !element <- findElement CssSelector "body"
      !screenshot <- takeElementScreenshot element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError

{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Web.Api.WebDriver.Monad.Test.Session.Success (
    successfulExit
  ) where

import System.IO

import Web.Api.WebDriver
import Test.Tasty.WebDriver

import qualified Test.Tasty as T
import qualified Test.Tasty.ExpectedFailure as TE


unexpectedError
  :: E WDError
  -> WebDriver ()
unexpectedError e = assertFailure $ AssertionComment $ "Unexpected error:\n" ++ show e


successfulExit
  :: (String -> WebDriver () -> T.TestTree)
  -> FilePath
  -> T.TestTree
successfulExit buildTestCase dir =
  let path = dir ++ "/success.html" in
  T.testGroup "Successful Exit"
    [ buildTestCase "sessionStatus" (_test_sessionStatus_success path)
    , buildTestCase "getTimeouts" (_test_getTimeouts_success)
    , buildTestCase "setTimeouts" (_test_setTimeouts_success)
    , buildTestCase "navigateTo" (_test_navigateTo_success path)
    , buildTestCase "navigateToStealth" (_test_navigateToStealth_success path)
    , buildTestCase "getCurrentUrl" (_test_getCurrentUrl_success)
    , buildTestCase "goBack" (_test_goBack_success)
    , buildTestCase "goForward" (_test_goForward_success)
    , buildTestCase "pageRefresh" (_test_pageRefresh_success)
    , buildTestCase "getTitle" (_test_getTitle_success)
    , buildTestCase "getWindowHandle" (_test_getWindowHandle_success)
    , buildTestCase "switchToWindow" (_test_switchToWindow_success)
    , buildTestCase "getWindowHandles" (_test_getWindowHandles_success path)
    , buildTestCase "switchToFrame" (_test_switchToFrame_success path)
    , buildTestCase "switchToParentFrame" (_test_switchToParentFrame_success path)
    , buildTestCase "getWindowRect" (_test_getWindowRect_success)
    , buildTestCase "setWindowRect" (_test_setWindowRect_success)
    ,   ifHeadless TE.ignoreTest
      $ buildTestCase "maximizeWindow" (_test_maximizeWindow_success)
    ,   ifHeadless TE.ignoreTest
      $ buildTestCase "minimizeWindow" (_test_minimizeWindow_success)
    ,   ifHeadless TE.ignoreTest
      $ buildTestCase "fullscreenWindow" (_test_fullscreenWindow_success)
    , buildTestCase "findElement" (_test_findElement_success path)
    , buildTestCase "findElements" (_test_findElements_success path)
    , buildTestCase "findElementFromElement" (_test_findElementFromElement_success path)
    , buildTestCase "findElementsFromElement" (_test_findElementsFromElement_success path)
    , buildTestCase "getActiveElement" (_test_getActiveElement_success)
    , buildTestCase "isElementSelected" (_test_isElementSelected_success path)
    , buildTestCase "getElementAttribute" (_test_getElementAttribute_success path)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ buildTestCase "getElementCssValue" (_test_getElementCssValue_success path)
    , buildTestCase "getElementText" (_test_getElementText_success path)
    , buildTestCase "getElementTagName" (_test_getElementTagName_success path)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ buildTestCase "getElementRect" (_test_getElementRect_success path)
    , buildTestCase "isElementEnabled" (_test_isElementEnabled_success path)
    , buildTestCase "elementClick" (_test_elementClick_success path)
    , buildTestCase "elementClear" (_test_elementClear_success path)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ buildTestCase "elementSendKeys" (_test_elementSendKeys_success path)
    , buildTestCase "getPageSource" (_test_getPageSource_success path)
    , buildTestCase "getPageSourceStealth" (_test_getPageSourceStealth_success path)
    , buildTestCase "getAllCookies" (_test_getAllCookies_success path)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ buildTestCase "getNamedCookie" (_test_getNamedCookie_success path)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ buildTestCase "deleteCookie" (_test_deleteCookie_success path)
    , buildTestCase "deleteAllCookies" (_test_deleteAllCookies_success)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ buildTestCase "performActions (keyboard)" (_test_performActions_keyboard_success)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ buildTestCase "performActionsStealth (keyboard)" (_test_performActionsStealth_keyboard_success)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ buildTestCase "releaseActions" (_test_releaseActions_success)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ buildTestCase "dismissAlert" (_test_dismissAlert_success path)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ buildTestCase "acceptAlert" (_test_acceptAlert_success path)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ ifHeadless TE.ignoreTest
      $ buildTestCase "getAlertText" (_test_getAlertText_success path)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ buildTestCase "sendAlertText" (_test_sendAlertText_success path)
    , buildTestCase "takeScreenshot" (_test_takeScreenshot_success path)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ buildTestCase "takeElementScreenshot" (_test_takeElementScreenshot_success path)
    ]



_test_sessionStatus_success
  :: FilePath -> WebDriver ()
_test_sessionStatus_success page =
  let
    session = do
      navigateTo page
      (!r,!m) <- sessionStatus
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_getTimeouts_success
  :: WebDriver ()
_test_getTimeouts_success =
  let
    session = do
      !timeouts <- getTimeouts
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_setTimeouts_success
  :: WebDriver ()
_test_setTimeouts_success =
  let
    session = do
      () <- setTimeouts emptyTimeoutConfig
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_navigateTo_success
  :: FilePath -> WebDriver ()
_test_navigateTo_success page =
  let
    session = do
      () <- navigateTo page
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_navigateToStealth_success
  :: FilePath -> WebDriver ()
_test_navigateToStealth_success page =
  let
    session = do
      () <- navigateToStealth page
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_getCurrentUrl_success
  :: WebDriver ()
_test_getCurrentUrl_success =
  let
    session = do
      !url <- getCurrentUrl
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_goBack_success
  :: WebDriver ()
_test_goBack_success =
  let
    session = do
      () <- goBack
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_goForward_success
  :: WebDriver ()
_test_goForward_success =
  let
    session = do
      () <- goForward
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_pageRefresh_success
  :: WebDriver ()
_test_pageRefresh_success =
  let
    session = do
      () <- pageRefresh
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_getTitle_success
  :: WebDriver ()
_test_getTitle_success =
  let
    session = do
      !title <- getTitle
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_getWindowHandle_success
  :: WebDriver ()
_test_getWindowHandle_success =
  let
    session = do
      !handle <- getWindowHandle
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



-- TODO: closeWindow



_test_switchToWindow_success
  :: WebDriver ()
_test_switchToWindow_success =
  let
    session = do
      hs <- getWindowHandles
      case hs of
        [] -> assertFailure "no window handles"
        (!h):_ -> do
          () <- switchToWindow h
          assertSuccess "yay"
          return ()

  in  catch session unexpectedError



_test_getWindowHandles_success
  :: FilePath -> WebDriver ()
_test_getWindowHandles_success page =
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

  in  catch session unexpectedError



_test_switchToFrame_success
  :: FilePath -> WebDriver ()
_test_switchToFrame_success page =
  let
    session = do
      navigateTo page
      () <- switchToFrame TopLevelFrame
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_switchToParentFrame_success
  :: FilePath -> WebDriver ()
_test_switchToParentFrame_success page =
  let
    session = do
      navigateTo page
      () <- switchToParentFrame
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_getWindowRect_success
  :: WebDriver ()
_test_getWindowRect_success =
  let
    session = do
      !rect <- getWindowRect
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_setWindowRect_success
  :: WebDriver ()
_test_setWindowRect_success =
  let
    session = do
      !rect <- setWindowRect $ Rect
        { _rectX = 0
        , _rectY = 0
        , _rectWidth = 640
        , _rectHeight = 480
        }
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_maximizeWindow_success
  :: WebDriver ()
_test_maximizeWindow_success =
  let
    session = do
      !rect <- maximizeWindow
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_minimizeWindow_success
  :: WebDriver ()
_test_minimizeWindow_success =
  let
    session = do
      !rect <- minimizeWindow
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_fullscreenWindow_success
  :: WebDriver ()
_test_fullscreenWindow_success =
  let
    session = do
      !rect <- fullscreenWindow
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_findElement_success
  :: FilePath -> WebDriver ()
_test_findElement_success page =
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

  in  catch session unexpectedError



_test_findElements_success
  :: FilePath -> WebDriver ()
_test_findElements_success page =
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

  in  catch session unexpectedError



_test_findElementFromElement_success
  :: FilePath -> WebDriver ()
_test_findElementFromElement_success page =
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

  in  catch session unexpectedError



_test_findElementsFromElement_success
  :: FilePath -> WebDriver ()
_test_findElementsFromElement_success page =
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

  in  catch session unexpectedError



_test_getActiveElement_success
  :: WebDriver ()
_test_getActiveElement_success =
  let
    session = do
      !element <- getActiveElement
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_isElementSelected_success
  :: FilePath -> WebDriver ()
_test_isElementSelected_success page =
  let
    session = do
      navigateTo page
      !element <- getActiveElement
      !p <- isElementSelected element
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_getElementAttribute_success
  :: FilePath -> WebDriver ()
_test_getElementAttribute_success page =
  let
    session = do
      navigateTo page
      !element <- getActiveElement
      !attr <- getElementAttribute element "href"
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



-- TODO: getElementProperty



_test_getElementCssValue_success
  :: FilePath -> WebDriver ()
_test_getElementCssValue_success page =
  let
    session = do
      navigateTo page
      !element <- findElement CssSelector "p#super-cool"
      !text <- getElementCssValue element "text-decoration"
      case text of
        "none" -> assertSuccess "yay"
        _ -> assertFailure $ AssertionComment $ "expected 'none', got '" ++ text ++ "'"
      return ()

  in  catch session unexpectedError



_test_getElementText_success
  :: FilePath -> WebDriver ()
_test_getElementText_success page =
  let
    session = do
      navigateTo page
      !element <- getActiveElement
      !text <- getElementText element
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_getElementTagName_success
  :: FilePath -> WebDriver ()
_test_getElementTagName_success page =
  let
    session = do
      navigateTo page
      !element <- findElement CssSelector "div.test"
      !text <- getElementTagName element
      case text of
        "div" -> assertSuccess "yay"
        _ -> assertFailure $ AssertionComment $ "expected 'div', got '" ++ text ++ "'"
      return ()

  in  catch session unexpectedError



_test_getElementRect_success
  :: FilePath -> WebDriver ()
_test_getElementRect_success page =
  let
    session = do
      navigateTo page
      !element <- getActiveElement
      !rect <- getElementRect element
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_isElementEnabled_success
  :: FilePath -> WebDriver ()
_test_isElementEnabled_success page =
  let
    session = do
      navigateTo page
      !element <- getActiveElement
      !p <- isElementEnabled element
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_elementClick_success
  :: FilePath -> WebDriver ()
_test_elementClick_success page =
  let
    session = do
      navigateTo page
      !root <- findElement CssSelector "body"
      () <- elementClick root
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_elementClear_success
  :: FilePath -> WebDriver ()
_test_elementClear_success page =
  let
    session = do
      navigateTo page
      !element <- findElement CssSelector "input[name='sometext']"
      () <- elementClear element
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_elementSendKeys_success
  :: FilePath -> WebDriver ()
_test_elementSendKeys_success page =
  let
    session = do
      navigateTo page
      !element <- findElement CssSelector "input[name='sometext']"
      () <- elementSendKeys element "foo"
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_getPageSource_success
  :: FilePath -> WebDriver ()
_test_getPageSource_success page =
  let
    session = do
      navigateTo page
      !src <- getPageSource
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_getPageSourceStealth_success
  :: FilePath -> WebDriver ()
_test_getPageSourceStealth_success page =
  let
    session = do
      navigateTo page
      !src <- getPageSourceStealth
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



-- TODO: executeScript



-- TODO: executeAsyncScript



_test_getAllCookies_success
  :: FilePath -> WebDriver ()
_test_getAllCookies_success page =
  let
    session = do
      navigateTo page
      !jar <- getAllCookies
      case jar of
        [] -> assertSuccess "yay"
        (!x):_ -> assertFailure "unexpected cookie"
      return ()

  in  catch session unexpectedError



_test_getNamedCookie_success
  :: FilePath -> WebDriver ()
_test_getNamedCookie_success page =
  let
    session = do
      navigateTo page
      findElement CssSelector "button#add-cookie-button" >>= elementClick
      !cookie <- getNamedCookie "fakeCookie"
      assertEqual (_cookieName cookie) (Just "fakeCookie") "cookie name"
      assertEqual (_cookieValue cookie) (Just "fakeValue") "cookie name"
      return ()

  in  catch session unexpectedError



{- TODO: addCookie -}
{- note: file:// addresses do not like cookies -}



_test_deleteCookie_success
  :: FilePath -> WebDriver ()
_test_deleteCookie_success page =
  let
    session = do
      navigateTo page
      findElement CssSelector "button#add-cookie-button" >>= elementClick
      () <- deleteCookie "fakeCookie"
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_deleteAllCookies_success
  :: WebDriver ()
_test_deleteAllCookies_success =
  let
    session = do
      () <- deleteAllCookies
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_performActions_keyboard_success
  :: WebDriver ()
_test_performActions_keyboard_success =
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

  in  catch session unexpectedError



_test_performActionsStealth_keyboard_success
  :: WebDriver ()
_test_performActionsStealth_keyboard_success =
  let
    session = do
      () <- performActionsStealth [ press EnterKey ]
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_releaseActions_success
  :: WebDriver ()
_test_releaseActions_success =
  let
    session = do
      () <- releaseActions
      assertSuccess "yay"
      return ()

  in  catch session unexpectedError



_test_dismissAlert_success
  :: FilePath -> WebDriver ()
_test_dismissAlert_success page =
  let
    session = do
      navigateTo page
      findElement CssSelector "button#alert-button" >>= elementClick
      () <- dismissAlert
      assertSuccess "yay alert"
      findElement CssSelector "button#confirm-button" >>= elementClick
      () <- dismissAlert
      assertSuccess "yay confirm"
      findElement CssSelector "button#prompt-button" >>= elementClick
      () <- dismissAlert
      assertSuccess "yay prompt"
      return ()

  in  catch session unexpectedError



_test_acceptAlert_success
  :: FilePath -> WebDriver ()
_test_acceptAlert_success page =
  let
    session = do
      navigateTo page
      findElement CssSelector "button#alert-button" >>= elementClick
      () <- acceptAlert
      assertSuccess "yay alert"
      findElement CssSelector "button#confirm-button" >>= elementClick
      () <- acceptAlert
      assertSuccess "yay confirm"
      findElement CssSelector "button#prompt-button" >>= elementClick
      () <- acceptAlert
      assertSuccess "yay prompt"
      return ()

  in  catch session unexpectedError



_test_getAlertText_success
  :: FilePath -> WebDriver ()
_test_getAlertText_success page =
  let
    session = do
      navigateTo page
      findElement CssSelector "button#alert-button" >>= elementClick
      !box <- getAlertText
      case box of
        Nothing -> assertFailure "oh no alert"
        Just msg -> assertEqual msg "WOO!!" "alert text"
      acceptAlert
      findElement CssSelector "button#confirm-button" >>= elementClick
      !box <- getAlertText
      case box of
        Nothing -> assertFailure "oh no confirm"
        Just msg -> assertEqual msg "WOO!!" "confirm text"
      acceptAlert
      findElement CssSelector "button#prompt-button" >>= elementClick
      !box <- getAlertText
      case box of
        Nothing -> assertFailure "oh no prompt"
        Just msg -> assertEqual msg "WOO!!" "prompt text"
      acceptAlert
      return ()

  in  catch session unexpectedError



_test_sendAlertText_success
  :: FilePath -> WebDriver ()
_test_sendAlertText_success page =
  let
    session = do
      navigateTo page
      findElement CssSelector "button#prompt-button" >>= elementClick
      () <- sendAlertText "wut"
      assertSuccess "yay prompt"
      return ()

  in  catch session unexpectedError



_test_takeScreenshot_success
  :: FilePath -> WebDriver ()
_test_takeScreenshot_success page =
  let
    session = do
      navigateTo page
      !screenshot <- takeScreenshot
      assertSuccess "yay"
      return ()

  in catch session unexpectedError



_test_takeElementScreenshot_success
  :: FilePath -> WebDriver ()
_test_takeElementScreenshot_success page =
  let
    session = do
      navigateTo page
      !element <- findElement CssSelector "body"
      !screenshot <- takeElementScreenshot element
      assertSuccess "yay"
      return ()

  in catch session unexpectedError

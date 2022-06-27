{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Web.Api.WebDriver.Monad.Test.Session.Success (
    successfulExit
  ) where

import Web.Api.WebDriver
import Test.Tasty.WebDriver

import qualified Test.Tasty as T
import qualified Test.Tasty.ExpectedFailure as TE

import qualified Data.Text as Text


unexpectedError
  :: (Monad eff)
  => WDError
  -> WebDriverT eff ()
unexpectedError e = assertFailure $ AssertionComment $
  "Unexpected error:\n" <> Text.pack (show e)


successfulExit
  :: (Monad eff)
  => (String -> WebDriverT eff () -> T.TestTree)
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
    , buildTestCase "newWindow" (_test_newWindow_success path)
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
    , buildTestCase "getElementCssValue" (_test_getElementCssValue_success path)
    , buildTestCase "getElementText" (_test_getElementText_success path)
    , buildTestCase "getElementTagName" (_test_getElementTagName_success path)
    , buildTestCase "getElementRect" (_test_getElementRect_success path)
    , buildTestCase "isElementEnabled" (_test_isElementEnabled_success path)
    ,   ifDriverIs Geckodriver TE.ignoreTest -- see https://bugzilla.mozilla.org/show_bug.cgi?id=1585622
      $ buildTestCase "getComputedRole" (_test_getComputedRole_success path)
    ,   ifDriverIs Geckodriver TE.ignoreTest -- see https://bugzilla.mozilla.org/show_bug.cgi?id=1585622
      $ buildTestCase "getComputedLabel" (_test_getComputedLabel_success path)
    , buildTestCase "elementClick" (_test_elementClick_success path)
    , buildTestCase "elementClear" (_test_elementClear_success path)
    , buildTestCase "elementSendKeys" (_test_elementSendKeys_success path)
    , buildTestCase "getPageSource" (_test_getPageSource_success path)
    , buildTestCase "getPageSourceStealth" (_test_getPageSourceStealth_success path)
    , buildTestCase "getAllCookies" (_test_getAllCookies_success path)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ T.localOption (PrivateMode False)
      $ buildTestCase "getNamedCookie" (_test_getNamedCookie_success path)
    , buildTestCase "deleteCookie" (_test_deleteCookie_success path)
    , buildTestCase "deleteAllCookies" (_test_deleteAllCookies_success)
    , buildTestCase "performActions (keyboard)" (_test_performActions_keyboard_success)
    , buildTestCase "performActionsStealth (keyboard)" (_test_performActionsStealth_keyboard_success)
    , buildTestCase "releaseActions" (_test_releaseActions_success)
    , buildTestCase "dismissAlert" (_test_dismissAlert_success path)
    , buildTestCase "acceptAlert" (_test_acceptAlert_success path)
    ,   ifDriverIs Chromedriver TE.ignoreTest
      $ ifHeadless TE.ignoreTest
      $ buildTestCase "getAlertText" (_test_getAlertText_success path)
    , buildTestCase "sendAlertText" (_test_sendAlertText_success path)
    , buildTestCase "takeScreenshot" (_test_takeScreenshot_success path)
    , buildTestCase "takeElementScreenshot" (_test_takeElementScreenshot_success path)
    , buildTestCase "printPage" (_test_printPage_success path)
    ]



_test_sessionStatus_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_sessionStatus_success page =
  let
    session = do
      navigateTo $ Text.pack page
      _ <- sessionStatus
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_getTimeouts_success
  :: (Monad eff) => WebDriverT eff ()
_test_getTimeouts_success =
  let
    session = do
      _ <- getTimeouts
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_setTimeouts_success
  :: (Monad eff) => WebDriverT eff ()
_test_setTimeouts_success =
  let
    session = do
      _ <- setTimeouts emptyTimeoutConfig
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_navigateTo_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_navigateTo_success page =
  let
    session = do
      _ <- navigateTo $ Text.pack page
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_navigateToStealth_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_navigateToStealth_success page =
  let
    session = do
      _ <- navigateToStealth $ Text.pack page
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_getCurrentUrl_success
  :: (Monad eff) => WebDriverT eff ()
_test_getCurrentUrl_success =
  let
    session = do
      _ <- getCurrentUrl
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_goBack_success
  :: (Monad eff) => WebDriverT eff ()
_test_goBack_success =
  let
    session = do
      _ <- navigateTo "https://example.com"
        -- chromedriver gets cranky if we try to navigate back when there is no history :)
      _ <- goBack
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_goForward_success
  :: (Monad eff) => WebDriverT eff ()
_test_goForward_success =
  let
    session = do
      _ <- goForward
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_pageRefresh_success
  :: (Monad eff) => WebDriverT eff ()
_test_pageRefresh_success =
  let
    session = do
      _ <- pageRefresh
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_getTitle_success
  :: (Monad eff) => WebDriverT eff ()
_test_getTitle_success =
  let
    session = do
      _ <- getTitle
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_getWindowHandle_success
  :: (Monad eff) => WebDriverT eff ()
_test_getWindowHandle_success =
  let
    session = do
      _ <- getWindowHandle
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



-- TODO: closeWindow



_test_switchToWindow_success
  :: (Monad eff) => WebDriverT eff ()
_test_switchToWindow_success =
  let
    session = do
      hs <- getWindowHandles
      case hs of
        [] -> assertFailure "no window handles"
        (!h):_ -> do
          _ <- switchToWindow h
          assertSuccess "yay"
          return ()

  in  catchError session unexpectedError



_test_getWindowHandles_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getWindowHandles_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !handles <- getWindowHandles
      case handles of
        [] -> do
          assertSuccess "yay"
          return ()
        _:_ -> do
          assertSuccess "yay"
          return ()

  in  catchError session unexpectedError



_test_newWindow_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_newWindow_success page =
  let
    session = do
      navigateTo $ Text.pack page
      (handle, _) <- newWindow TabContext
      switchToWindow handle
      url <- getCurrentUrl
      assertEqual url "about:blank" "default url"
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_switchToFrame_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_switchToFrame_success page =
  let
    session = do
      navigateTo $ Text.pack page
      _ <- switchToFrame TopLevelFrame
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_switchToParentFrame_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_switchToParentFrame_success page =
  let
    session = do
      navigateTo $ Text.pack page
      () <- switchToParentFrame
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_getWindowRect_success
  :: (Monad eff) => WebDriverT eff ()
_test_getWindowRect_success =
  let
    session = do
      _ <- getWindowRect
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_setWindowRect_success
  :: (Monad eff) => WebDriverT eff ()
_test_setWindowRect_success =
  let
    session = do
      _ <- setWindowRect $ Rect
        { _rectX = 0
        , _rectY = 0
        , _rectWidth = 640
        , _rectHeight = 480
        }
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_maximizeWindow_success
  :: (Monad eff) => WebDriverT eff ()
_test_maximizeWindow_success =
  let
    session = do
      _ <- maximizeWindow
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_minimizeWindow_success
  :: (Monad eff) => WebDriverT eff ()
_test_minimizeWindow_success =
  let
    session = do
      _ <- minimizeWindow
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_fullscreenWindow_success
  :: (Monad eff) => WebDriverT eff ()
_test_fullscreenWindow_success =
  let
    session = do
      _ <- fullscreenWindow
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_findElement_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_findElement_success page =
  let
    session = do
      navigateTo $ Text.pack page
      _ <- findElement CssSelector "body"
      _ <- findElement LinkTextSelector "A Link"
      _ <- findElement PartialLinkTextSelector "Link"
      _ <- findElement TagName "body"
      _ <- findElement XPathSelector "*"
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_findElements_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_findElements_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !e0 <- findElements CssSelector "body"
      case e0 of
        [] -> return ()
        _:_ -> return ()
      !e1 <- findElements LinkTextSelector "Standards"
      case e1 of
        [] -> return ()
        _:_ -> return ()
      !e2 <- findElements PartialLinkTextSelector "Standards"
      case e2 of
        [] -> return ()
        _:_ -> return ()
      !e3 <- findElements TagName "body"
      case e3 of
        [] -> return ()
        _:_ -> return ()
      !e4 <- findElements XPathSelector "*"
      case e4 of
        [] -> return ()
        _:_ -> return ()
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_findElementFromElement_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_findElementFromElement_success page =
  let
    session = do
      navigateTo $ Text.pack page
      root <- findElement CssSelector "body"
      _ <- findElementFromElement CssSelector "p" root
      _ <- findElementFromElement LinkTextSelector "A Link" root
      _ <- findElementFromElement PartialLinkTextSelector "Link" root
      _ <- findElementFromElement TagName "p" root
      _ <- findElementFromElement XPathSelector "*" root
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_findElementsFromElement_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_findElementsFromElement_success page =
  let
    session = do
      navigateTo $ Text.pack page
      root <- findElement CssSelector "body"
      !e0 <- findElementsFromElement CssSelector "p" root
      case e0 of
        [] -> return ()
        _:_ -> return ()
      !e1 <- findElementsFromElement LinkTextSelector "Standards" root
      case e1 of
        [] -> return ()
        _:_ -> return ()
      !e2 <- findElementsFromElement PartialLinkTextSelector "Standards" root
      case e2 of
        [] -> return ()
        _:_ -> return ()
      !e3 <- findElementsFromElement TagName "p" root
      case e3 of
        [] -> return ()
        _:_ -> return ()
      !e4 <- findElementsFromElement XPathSelector "*" root
      case e4 of
        [] -> return ()
        _:_ -> return ()
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_getActiveElement_success
  :: (Monad eff) => WebDriverT eff ()
_test_getActiveElement_success =
  let
    session = do
      _ <- getActiveElement
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_isElementSelected_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_isElementSelected_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !element <- getActiveElement
      _ <- isElementSelected element
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_getElementAttribute_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getElementAttribute_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !element <- getActiveElement
      _ <- getElementAttribute "href" element
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



-- TODO: getElementProperty



_test_getElementCssValue_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getElementCssValue_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !element <- findElement CssSelector "p#super-cool"
      !text <- getElementCssValue "text-decoration" element
      case text of
        "none" -> assertSuccess "yay"
        "rgb(0, 0, 0)" -> assertSuccess "yay"
        "none solid rgb(0, 0, 0)" -> assertSuccess "yay"
        _ -> assertFailure $ AssertionComment $ mconcat
          [ "expected 'none' or 'rgb(0, 0, 0)' or 'none solid rgb(0, 0, 0)', got '"
          , text, "'"
          ]
      return ()

  in  catchError session unexpectedError



_test_getElementText_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getElementText_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !element <- getActiveElement
      _ <- getElementText element
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_getElementTagName_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getElementTagName_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !element <- findElement CssSelector "div.test"
      !text <- getElementTagName element
      case text of
        "div" -> assertSuccess "yay"
        _ -> assertFailure $ AssertionComment $
          "expected 'div', got '" <> text <> "'"
      return ()

  in  catchError session unexpectedError



_test_getElementRect_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getElementRect_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !element <- getActiveElement
      _ <- getElementRect element
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_isElementEnabled_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_isElementEnabled_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !element <- getActiveElement
      _ <- isElementEnabled element
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_getComputedRole_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getComputedRole_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !element <- getActiveElement
      _ <- getComputedRole element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_getComputedLabel_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getComputedLabel_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !element <- getActiveElement
      _ <- getComputedLabel element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_elementClick_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_elementClick_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !root <- findElement CssSelector "body"
      _ <- elementClick root
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_elementClear_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_elementClear_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !element <- findElement CssSelector "input[name='sometext']"
      _ <- elementClear element
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_elementSendKeys_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_elementSendKeys_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !element <- findElement CssSelector "input[name='sometext']"
      _ <- elementSendKeys "foo" element
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_getPageSource_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getPageSource_success page =
  let
    session = do
      navigateTo $ Text.pack page
      _ <- getPageSource
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_getPageSourceStealth_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getPageSourceStealth_success page =
  let
    session = do
      navigateTo $ Text.pack page
      _ <- getPageSourceStealth
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



-- TODO: executeScript



-- TODO: executeAsyncScript



_test_getAllCookies_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getAllCookies_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !jar <- getAllCookies
      case jar of
        [] -> assertSuccess "yay"
        _:_ -> assertFailure "unexpected cookie"
      return ()

  in  catchError session unexpectedError



_test_getNamedCookie_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getNamedCookie_success page =
  let
    session = do
      navigateTo $ Text.pack page
      findElement CssSelector "button#add-cookie-button" >>= elementClick
      !c <- getNamedCookie "fakeCookie"
      assertEqual (_cookieName c) (Just "fakeCookie") "cookie name"
      assertEqual (_cookieValue c) (Just "fakeValue") "cookie name"
      return ()

  in  catchError session unexpectedError



{- TODO: addCookie -}
{- note: file:// addresses do not like cookies -}



_test_deleteCookie_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_deleteCookie_success page =
  let
    session = do
      navigateTo $ Text.pack page
      findElement CssSelector "button#add-cookie-button" >>= elementClick
      () <- deleteCookie "fakeCookie"
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_deleteAllCookies_success
  :: (Monad eff) => WebDriverT eff ()
_test_deleteAllCookies_success =
  let
    session = do
      () <- deleteAllCookies
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_performActions_keyboard_success
  :: (Monad eff) => WebDriverT eff ()
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

  in  catchError session unexpectedError



_test_performActionsStealth_keyboard_success
  :: (Monad eff) => WebDriverT eff ()
_test_performActionsStealth_keyboard_success =
  let
    session = do
      () <- performActionsStealth [ press EnterKey ]
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_releaseActions_success
  :: (Monad eff) => WebDriverT eff ()
_test_releaseActions_success =
  let
    session = do
      () <- releaseActions
      assertSuccess "yay"
      return ()

  in  catchError session unexpectedError



_test_dismissAlert_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_dismissAlert_success page =
  let
    session = do
      navigateTo $ Text.pack page
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

  in  catchError session unexpectedError



_test_acceptAlert_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_acceptAlert_success page =
  let
    session = do
      navigateTo $ Text.pack page
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

  in  catchError session unexpectedError



_test_getAlertText_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_getAlertText_success page =
  let
    session = do
      navigateTo $ Text.pack page
      findElement CssSelector "button#alert-button" >>= elementClick
      !box0 <- getAlertText
      case box0 of
        Nothing -> assertFailure "oh no alert"
        Just msg -> assertEqual msg "WOO!!" "alert text"
      acceptAlert
      findElement CssSelector "button#confirm-button" >>= elementClick
      !box1 <- getAlertText
      case box1 of
        Nothing -> assertFailure "oh no confirm"
        Just msg -> assertEqual msg "WOO!!" "confirm text"
      acceptAlert
      findElement CssSelector "button#prompt-button" >>= elementClick
      !box2 <- getAlertText
      case box2 of
        Nothing -> assertFailure "oh no prompt"
        Just msg -> assertEqual msg "WOO!!" "prompt text"
      acceptAlert
      return ()

  in  catchError session unexpectedError



_test_sendAlertText_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_sendAlertText_success page =
  let
    session = do
      navigateTo $ Text.pack page
      findElement CssSelector "button#prompt-button" >>= elementClick
      () <- sendAlertText "wut"
      assertSuccess "yay prompt"
      return ()

  in  catchError session unexpectedError



_test_takeScreenshot_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_takeScreenshot_success page =
  let
    session = do
      navigateTo $ Text.pack page
      _ <- takeScreenshot
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_takeElementScreenshot_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_takeElementScreenshot_success page =
  let
    session = do
      navigateTo $ Text.pack page
      !element <- findElement CssSelector "body"
      _ <- takeElementScreenshot element
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError



_test_printPage_success
  :: (Monad eff) => FilePath -> WebDriverT eff ()
_test_printPage_success page =
  let
    session = do
      navigateTo $ Text.pack page
      _ <- printPage defaultPrintOptions
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError

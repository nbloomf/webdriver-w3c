{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Web.Api.WebDriver.Monad.Test.Session.InvalidElementState (
    invalidElementStateExit
  ) where

import Data.Typeable (Typeable)
import System.IO


import Web.Api.WebDriver
import Test.Tasty.WebDriver

import qualified Data.Text as Text

import qualified Test.Tasty as T
import qualified Test.Tasty.ExpectedFailure as TE


invalidElementState
  :: (Monad eff)
  => WDError
  -> WebDriverT eff()
invalidElementState e = case e of
  ResponseError InvalidElementState _ _ _ _ -> assertSuccess "yay!"
  err -> assertFailure $ AssertionComment $
    "Expecting 'invalid element state' but got: " <> Text.pack (show err)


invalidElementStateExit
  :: (Monad eff)
  => (String -> WebDriverT eff() -> T.TestTree)
  -> FilePath
  -> T.TestTree
invalidElementStateExit buildTestCase dir =
  let path = dir ++ "/invalidElementState.html" in
  T.testGroup "Invalid Element State"
    [ buildTestCase "elementClear" (_test_elementClear_invalid_element_state path)
    ]



_test_elementClear_invalid_element_state
  :: (Monad eff) => FilePath -> WebDriverT eff()
_test_elementClear_invalid_element_state page =
  let
    session :: (Monad eff) => WebDriverT eff()
    session = do
      navigateTo $ Text.pack page
      !element <- findElement CssSelector "body"
      elementClear element
      throwError $ UnexpectedResult IsSuccess "Expecting 'invalid_element_state'"
      return ()

  in catchError session invalidElementState

{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Web.Api.WebDriver.Monad.Test.Session.InvalidElementState (
    invalidElementStateExit
  ) where

import Data.Typeable (Typeable)
import System.IO


import Web.Api.WebDriver
import Test.Tasty.WebDriver

import qualified Test.Tasty as T
import qualified Test.Tasty.ExpectedFailure as TE


invalidElementState
  :: E WDError
  -> WebDriver ()
invalidElementState e = case e of
  E (ResponseError InvalidElementState _ _ _ _) -> assertSuccess "yay!"
  err -> assertFailure $
    AssertionComment $ "Expecting 'invalid element state' but got: " ++ show err


invalidElementStateExit
  :: (String -> WebDriver () -> T.TestTree)
  -> FilePath
  -> T.TestTree
invalidElementStateExit buildTestCase dir =
  let path = dir ++ "/invalidElementState.html" in
  T.testGroup "Invalid Element State"
    [ ifDriverIs Chromedriver TE.ignoreTest $
        buildTestCase "elementClear" (_test_elementClear_invalid_element_state path)
    ]



_test_elementClear_invalid_element_state
  :: FilePath -> WebDriver ()
_test_elementClear_invalid_element_state page =
  let
    session :: WebDriver ()
    session = do
      navigateTo page
      !element <- findElement CssSelector "body"
      elementClear element
      throwError $ E $ UnexpectedResult IsSuccess "Expecting 'invalid_element_state'"
      return ()

  in catch session invalidElementState

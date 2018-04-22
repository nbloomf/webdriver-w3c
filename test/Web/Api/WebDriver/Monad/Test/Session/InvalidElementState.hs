{-# LANGUAGE BangPatterns #-}
module Web.Api.WebDriver.Monad.Test.Session.InvalidElementState (
    invalidElementStateExit
  ) where

import Data.Typeable (Typeable)
import System.IO

import Web.Api.Http
import Web.Api.WebDriver
import Test.Tasty.WebDriver

import qualified Test.Tasty as T


invalidElementState
  :: (Effectful m, Typeable m)
  => Err WebDriverError
  -> WebDriver m ()
invalidElementState e = case e of
  Err (ResponseError InvalidElementState _ _ _ _) -> assertSuccess "yay!"
  _ -> assertFailure "Expecting 'invalid element state'"


invalidElementStateExit :: (Effectful m, Typeable m) => FilePath -> m () -> T.TestTree
invalidElementStateExit dir x =
  let path = dir ++ "/invalidElementState.html" in
  T.testGroup "Unknown Error"
    [ testCase "elementClear" (_test_elementClear_invalid_element_state path x)
    ]



_test_elementClear_invalid_element_state
  :: (Effectful m, Typeable m) => FilePath -> m () -> WebDriver m ()
_test_elementClear_invalid_element_state page _ =
  let
    session = do
      navigateTo page
      !element <- findElement CssSelector "body"
      elementClear element
      throwError $ ErrUnexpectedSuccess "Expecting 'invalid_element_state'"
      return ()

  in catchError session invalidElementState

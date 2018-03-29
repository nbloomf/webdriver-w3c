{-# LANGUAGE BangPatterns #-}
module Web.Api.WebDriver.Monad.Test.Session.UnknownError (
    unknownErrorExit
  ) where

import Data.Typeable (Typeable)

import Web.Api.Http
import Web.Api.WebDriver
import Test.Tasty.WebDriver

import qualified Test.Tasty as T


unknownError
  :: (Effectful m, Typeable m)
  => Err WebDriverError
  -> WebDriver m ()
unknownError e = case e of
  Err (ResponseError UnknownError _ _ _ _) -> assertSuccess "yay!"
  _ -> assertFailure "Expecting 'unknown error'"


unknownErrorExit :: (Effectful m, Typeable m) => m () -> T.TestTree
unknownErrorExit x = T.testGroup "Unknown Error"
  [ testCase "navigateTo" (_test_navigateTo_unknown_error x)
  ]



_test_navigateTo_unknown_error
  :: (Effectful m, Typeable m) => m () -> WebDriver m ()
_test_navigateTo_unknown_error _ =
  let
    session = do
      navigateTo "https://fake.example"
      throwError $ ErrUnexpectedSuccess "Expecting 'unknown error'"
      return ()

  in catchError session unknownError

{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Web.Api.WebDriver.Monad.Test.Session.UnknownError (
    unknownErrorExit
  ) where

import System.IO

import Web.Api.WebDriver
import Test.Tasty.WebDriver

import qualified Test.Tasty as T
import qualified Test.Tasty.ExpectedFailure as TE


unknownError
  :: (Monad eff)
  => WDError
  -> WebDriverT eff ()
unknownError e = case e of
  ResponseError UnknownError _ _ _ _ -> assertSuccess "yay!"
  _ -> assertFailure "Expecting 'unknown error'"


unknownErrorExit
  :: (Monad eff) => (String -> WebDriverT eff () -> T.TestTree)
  -> FilePath
  -> T.TestTree
unknownErrorExit buildTestCase path = T.testGroup "Unknown Error"
  [ buildTestCase "navigateTo" (_test_navigateTo_unknown_error)
  ]



_test_navigateTo_unknown_error
  :: (Monad eff) => WebDriverT eff ()
_test_navigateTo_unknown_error =
  let
    session = do
      navigateTo "https://fake.example"
      _ <- throwError $ UnexpectedResult IsSuccess "Expecting 'unknown error'"
      return ()

  in catchError session unknownError

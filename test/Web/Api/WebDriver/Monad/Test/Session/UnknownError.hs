{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Web.Api.WebDriver.Monad.Test.Session.UnknownError (
    unknownErrorExit
  ) where

import Data.Typeable (Typeable)
import System.IO

import Web.Api.WebDriver
import Test.Tasty.WebDriver

import qualified Test.Tasty as T
import qualified Test.Tasty.ExpectedFailure as TE


unknownError
  :: WDError
  -> WebDriver ()
unknownError e = case e of
  ResponseError UnknownError _ _ _ _ -> assertSuccess "yay!"
  _ -> assertFailure "Expecting 'unknown error'"


unknownErrorExit
  :: (String -> WebDriver () -> T.TestTree)
  -> FilePath
  -> T.TestTree
unknownErrorExit buildTestCase path = T.testGroup "Unknown Error"
  [ ifDriverIs Chromedriver TE.ignoreTest $
      buildTestCase "navigateTo" (_test_navigateTo_unknown_error)
  ]



_test_navigateTo_unknown_error
  :: WebDriver ()
_test_navigateTo_unknown_error =
  let
    session = do
      navigateTo "https://fake.example"
      throwError $ UnexpectedResult IsSuccess "Expecting 'unknown error'"
      return ()

  in catchError session unknownError

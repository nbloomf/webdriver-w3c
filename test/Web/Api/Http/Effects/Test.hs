{-# LANGUAGE FlexibleInstances, BangPatterns #-}
module Web.Api.Http.Effects.Test (
    tests
  ) where

import Data.Typeable
  ( Typeable )
import qualified Network.Wreq.Session as WreqS
  ( newSession )

import Test.Tasty (TestTree(), testGroup, localOption)
import Test.Tasty.QuickCheck as QC (testProperty)
import qualified Test.Tasty.HUnit as HU

import Web.Api.Http
import Web.Api.Http.Effects.Test.Mock
import Web.Api.Http.Effects.Test.Server
import Test.Tasty.HttpSession



tests :: TestTree
tests = testGroup "Web.Api.Http.Effects"
  [ testGroup "MockServer"
      (effectTests (return () :: MockIO () ()))

  ,   localOption (LogHandle $ Path "/dev/null")
    $ localOption (AssertionLogHandle $ Path "/dev/null")
    $ localOption (ConsoleOutHandle $ Path "/dev/null")
    $ testGroup "Real Server" (effectTests (return () :: IO ()))
  ]

effectTests :: (Effectful m, Typeable m) => m () -> [TestTree]
effectTests x =
  [ testCase "mPauseInSeconds" (_test_mPauseInSeconds_success x)
  , testCase "mPutStr" (_test_mPutStr_success x)
  , testCase "mRandomDecimalDigit" (_test_mRandomDecimalDigit_success x)
  , testCase "mRandomLowerCaseLetter" (_test_mRandomLowerCaseLetter_success x)
  , testCase "mRandomUpperCaseLetter" (_test_mRandomUpperCaseLetter_success x)
  , testCase "mRandomAlphanumericCharacter" (_test_mRandomAlphanumericCharacter_success x)
  , testCase "mRandomDecimalDigitString" (_test_mRandomDecimalDigitString_success x)
  , testCase "mRandomAlphanumericString" (_test_mRandomAlphanumericString_success x)
  ]

instance Effectful (MockIO ()) where
  toIO x = do
    session <- WreqS.newSession
    let
      init = mockSt defaultHttpSessionServer
        session ()

      (a,_) = runMockIO x init
    return a

defaultHttpSessionServer :: MockServer ()
defaultHttpSessionServer = MockServer
  { __http_get = \url -> undefined

  , __http_post = \url payload -> undefined

  , __http_delete = \url -> undefined
  }

unexpectedError
  :: (Effectful m, Typeable m)
  => Err ()
  -> HttpSession m () () () () ()
unexpectedError e = assertFailure $ "Unexpected error:\n" ++ show e

_test_mPauseInSeconds_success
  :: (Effectful m, Typeable m) => m () -> HttpSession m () () () () ()
_test_mPauseInSeconds_success _ =
  let
    session = do
      () <- mPauseInSeconds 1
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError


_test_mPutStr_success
  :: (Effectful m, Typeable m) => m () -> HttpSession m () () () () ()
_test_mPutStr_success _ =
  let
    session = do
      () <- mPutStr "some text"
      assertSuccess "yay"
      return ()

  in catchError session unexpectedError


_test_mRandomDecimalDigit_success
  :: (Effectful m, Typeable m) => m () -> HttpSession m () () () () ()
_test_mRandomDecimalDigit_success _ =
  let
    check = do
      c <- mRandomDecimalDigit
      if elem c ['0'..'9']
        then assertSuccess "yay"
        else do
          assertFailure $ "Expected a decimal digit; got '" ++ [c] ++ "'"
          throwError $ ErrUnexpectedFailure "mRandomDecimalDigit: bad characters."
      return ()

    session = sequence_ [check | i <- [1..1000]]

  in catchError session unexpectedError


_test_mRandomLowerCaseLetter_success
  :: (Effectful m, Typeable m) => m () -> HttpSession m () () () () ()
_test_mRandomLowerCaseLetter_success _ =
  let
    check = do
      c <- mRandomLowerCaseLetter
      if elem c ['a'..'z']
        then assertSuccess "yay"
        else do
          assertFailure $ "Expected a lower case letter; got '" ++ [c] ++ "'"
          throwError $ ErrUnexpectedFailure "mRandomLowerCaseLetter: bad characters."
      return ()

    session = sequence_ [check | i <- [1..1000]]

  in catchError session unexpectedError


_test_mRandomUpperCaseLetter_success
  :: (Effectful m, Typeable m) => m () -> HttpSession m () () () () ()
_test_mRandomUpperCaseLetter_success _ =
  let
    check = do
      c <- mRandomUpperCaseLetter
      if elem c ['A'..'Z']
        then assertSuccess "yay"
        else do
          assertFailure $ "Expected an upper case letter; got '" ++ [c] ++ "'"
          throwError $ ErrUnexpectedFailure "mRandomUpperCaseLetter: bad characters."
      return ()

    session = sequence_ [check | i <- [1..1000]]

  in catchError session unexpectedError


_test_mRandomAlphanumericCharacter_success
  :: (Effectful m, Typeable m) => m () -> HttpSession m () () () () ()
_test_mRandomAlphanumericCharacter_success _ =
  let
    check = do
      c <- mRandomAlphanumericCharacter
      if elem c $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
        then assertSuccess "yay"
        else do
          assertFailure $ "Expected an alphanumeric character; got '" ++ [c] ++ "'"
          throwError $ ErrUnexpectedFailure "mRandomAlphanumericCharacter: bad characters."
      return ()

    session = sequence_ [check | i <- [1..1000]]

  in catchError session unexpectedError


_test_mRandomDecimalDigitString_success
  :: (Effectful m, Typeable m) => m () -> HttpSession m () () () () ()
_test_mRandomDecimalDigitString_success _ =
  let
    check = do
      cs <- mRandomDecimalDigitString 8
      if all (`elem` ['0'..'9']) cs
        then assertSuccess "yay"
        else do
          assertFailure $ "Expected only decimal digits; got '" ++ cs ++ "'"
          throwError $ ErrUnexpectedFailure "mRandomDecimalDigitString: bad characters."
      return ()

    session = sequence_ [check | i <- [1..1000]]

  in catchError session unexpectedError


_test_mRandomAlphanumericString_success
  :: (Effectful m, Typeable m) => m () -> HttpSession m () () () () ()
_test_mRandomAlphanumericString_success _ =
  let
    check = do
      cs <- mRandomAlphanumericString 8
      if all (`elem` (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])) cs
        then assertSuccess "yay"
        else do
          assertFailure $ "Expected only alphanumeric characters; got '" ++ cs ++ "'"
          throwError $ ErrUnexpectedFailure "mRandomAlphanumericString: bad characters."
      return ()

    session = sequence_ [check | i <- [1..1000]]

  in catchError session unexpectedError

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
    $ testGroup "Real Server" (effectTests (return () :: IO ()))
  ]

effectTests :: (Effectful m, Typeable m) => m () -> [TestTree]
effectTests x =
  [ testCase "mPauseInSeconds" (_test_mPauseInSeconds_success x)
  , testCase "mPutStr" (_test_mPutStr_success x)
  , testCase "mRandomDecimalDigit" (_test_mRandomDecimalDigit_success x)
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
  { __http_get = \st !url -> undefined

  , __http_post = \st !url !payload -> undefined

  , __http_delete = \st !url -> undefined
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
        else assertFailure $ "Expected a decimal digit; got '" ++ [c] ++ "'"
      return ()

    session = sequence_ [check | i <- [1..100]]

  in catchError session unexpectedError

{-# LANGUAGE OverloadedStrings #-}
module Web.Api.Http.Assert.Test (
    tests
  ) where

import System.IO

import qualified Test.Tasty as TT (TestTree(), testGroup)
import qualified Test.Tasty.QuickCheck as QC (testProperty)
import qualified Test.Tasty.HUnit as HU

import Web.Api.Http

tests :: TT.TestTree
tests = TT.testGroup "Web.Api.Http.Assert"
  [ check_assertions
      "assertions"
      (\h -> setEnvironment (setLogHandle h . setAssertionLogHandle h)
        (basicHttpSessionConfig show show Nothing () ()))

  , check_assertion_summaries
      "assertion summaries"
      (\h -> setEnvironment (setLogHandle h . setAssertionLogHandle h)
        (basicHttpSessionConfig show show Nothing () ()))
  ]

check_assertion_summary
  :: (Handle -> HttpSessionConfig () () () ())
  -> HttpSession IO () () () () ()
  -> AssertionSummary
  -> IO ()
check_assertion_summary config session expect = do
  h <- openFile "/dev/null" WriteMode
  (_, result) <- debugSession (config h) session
  if summarize result == expect
    then return ()
    else HU.assertFailure $ "\n\ngot:\n " ++ show result ++ "\n\nbut expected:\n" ++ show expect

check_assertion_summaries
  :: String
  -> (Handle -> HttpSessionConfig () () () ())
  -> TT.TestTree
check_assertion_summaries name config = TT.testGroup ("assertion summary: " ++ name) $
  map
    (\(title,session,expect) -> HU.testCase title $ check_assertion_summary config session expect)
    _assertion_summary_cases

_assertion_summary_cases :: [(String, HttpSession IO () () () () (), AssertionSummary)]
_assertion_summary_cases =
  [ ( "assertSuccess"
    , do
        assertSuccess "yay!"
    , summarize [success "Success!" "yay!"]
    )

  , ( "assertFailure"
    , do
        assertFailure "oh no"
    , summarize [failure "Failure :(" "oh no"]
    )
  ]

check_assertion
  :: (Handle -> HttpSessionConfig () () () ())
  -> HttpSession IO () () () () ()
  -> [Assertion]
  -> IO ()
check_assertion config session expect = do
  h <- openFile "/dev/null" WriteMode
  (_, result) <- debugSession (config h) session
  if result == expect
    then return ()
    else HU.assertFailure $ "\n\ngot:\n " ++ show result ++ "\n\nbut expected:\n" ++ show expect

check_assertions
  :: String
  -> (Handle -> HttpSessionConfig () () () ())
  -> TT.TestTree
check_assertions name config = TT.testGroup ("assertions: " ++ name) $
  map
    (\(title,session,expect) -> HU.testCase title $ check_assertion config session expect)
    _assertion_cases

_assertion_cases :: [(String, HttpSession IO () () () () (), [Assertion])]
_assertion_cases =
  [ ( "assertSuccess"
    , do
        assertSuccess "yay!"
    , [success "Success!" "yay!"]
    )

  , ( "assertFailure"
    , do
        assertFailure "oh no"
    , [failure "Failure :(" "oh no"]
    )

  , ( "assertTrue (success)"
    , do
        assertTrue True "test"
    , [success "True is True" "test"]
    )

  , ( "assertTrue (failure)"
    , do
        assertTrue False "test"
    , [failure "False is True" "test"]
    )

  , ( "assertFalse (success)"
    , do
        assertFalse False "test"
    , [success "False is False" "test"]
    )

  , ( "assertFalse (failure)"
    , do
        assertFalse True "test"
    , [failure "True is False" "test"]
    )

  , ( "assertEqual (Int, success)"
    , do
        assertEqual (1::Int) (1::Int) "test"
    , [success "1 is equal to 1" "test"]
    )

  , ( "assertEqual (Int, failure)"
    , do
        assertEqual (2::Int) (1::Int) "test"
    , [failure "2 is equal to 1" "test"]
    )

  , ( "assertEqual (String, success)"
    , do
        assertEqual "A" "A" "test"
    , [success "\"A\" is equal to \"A\"" "test"]
    )

  , ( "assertEqual (String, failure)"
    , do
        assertEqual "B" "A" "test"
    , [failure "\"B\" is equal to \"A\"" "test"]
    )

  , ( "assertNotEqual (Int, success)"
    , do
        assertNotEqual (2::Int) (1::Int) "test"
    , [success "2 is not equal to 1" "test"]
    )

  , ( "assertNotEqual (Int, failure)"
    , do
        assertNotEqual (1::Int) (1::Int) "test"
    , [failure "1 is not equal to 1" "test"]
    )

  , ( "assertNotEqual (String, success)"
    , do
        assertNotEqual "B" "A" "test"
    , [success "\"B\" is not equal to \"A\"" "test"]
    )

  , ( "assertNotEqual (String, failure)"
    , do
        assertNotEqual "A" "A" "test"
    , [failure "\"A\" is not equal to \"A\"" "test"]
    )

  , ( "assertIsSubstring (success)"
    , do
        assertIsSubstring "oba" "foobar" "test"
    , [success "\"oba\" is a substring of \"foobar\"" "test"]
    )

  , ( "assertIsSubstring (failure)"
    , do
        assertIsSubstring "quux" "foobar" "test"
    , [failure "\"quux\" is a substring of \"foobar\"" "test"]
    )

  , ( "assertIsNotSubstring (success)"
    , do
        assertIsNotSubstring "quux" "foobar" "test"
    , [success "\"quux\" is not a substring of \"foobar\"" "test"]
    )

  , ( "assertIsNotSubstring (failure)"
    , do
        assertIsNotSubstring "oba" "foobar" "test"
    , [failure "\"oba\" is not a substring of \"foobar\"" "test"]
    )

  , ( "assertIsNamedSubstring (success)"
    , do
        assertIsNamedSubstring "oba" ("foobar","string") "test"
    , [success "\"oba\" is a substring of string" "test"]
    )

  , ( "assertIsNamedSubstring (failure)"
    , do
        assertIsNamedSubstring "quux" ("foobar","string") "test"
    , [failure "\"quux\" is a substring of string" "test"]
    )

  , ( "assertIsNotNamedSubstring (success)"
    , do
        assertIsNotNamedSubstring "quux" ("foobar","string") "test"
    , [success "\"quux\" is not a substring of string" "test"]
    )

  , ( "assertIsNotNamedSubstring (failure)"
    , do
        assertIsNotNamedSubstring "oba" ("foobar","string") "test"
    , [failure "\"oba\" is not a substring of string" "test"]
    )
  ]

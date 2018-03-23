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
      (\h -> setEnv (setLogHandle h . setAssertionLogHandle h)
        (basicHttpSessionConfig show show Nothing () ()))
  ]

check_assertion
  :: (Handle -> HttpSessionConfig () () () ())
  -> HttpSession IO () () () () ()
  -> [Assertion]
  -> IO ()
check_assertion config session expect = do
  h <- openFile "/dev/null" WriteMode
  result <- debugSession (config h) session
  if result == expect
    then return ()
    else HU.assertFailure $ "got: " ++ show result ++ "but expected: " ++ show expect

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
    , [success "Success!" "" "yay!"]
    )

  , ( "assertFailure"
    , do
        assertFailure "oh no"
    , [failure "Failure :(" "" "oh no"]
    )
  ]

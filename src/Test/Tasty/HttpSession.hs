{- |
Module      : Test.Tasty.HttpSession
Description : Generic HTTP session integration with the Tasty test framework.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Test.Tasty.HttpSession (
  -- * Test Case Constructors
    testCase
  , testCaseWithSetup

  -- * Options
  , LogHandle(..)
  , AssertionLogHandle(..)
  , FileHandle(..)
  ) where

import Data.Typeable
  ( Typeable, Proxy(Proxy) )
import System.IO
  ( Handle, stdout, stderr, openFile, IOMode(..) )

import qualified Test.Tasty.Providers as TT
import qualified Test.Tasty.Options as TO

import Web.Api.Http


data HttpSessionTest m = HttpSessionTest
  { _test_name :: Maybe String
  , _test_session :: (HttpSession m () () () () ())
  } deriving Typeable


instance (Effectful m, Typeable m) => TT.IsTest (HttpSessionTest m) where

  testOptions = return
    [ TO.Option (Proxy :: Proxy LogHandle)
    , TO.Option (Proxy :: Proxy AssertionLogHandle)
    ]

  run opts HttpSessionTest{..} _ = do
    let
      LogHandle log = TO.lookupOption opts
      AssertionLogHandle alog = TO.lookupOption opts

      title = case _test_name of
        Nothing -> return ()
        Just str -> comment str

    logHandle <- writeModeHandle log
    alogHandle <- writeModeHandle alog

    let
      config =
        setEnv
          ( setLogHandle logHandle
          . setAssertionLogHandle alogHandle
          ) $ basicHttpSessionConfig (const "") (const "") Nothing () ()

    (result, assertions) <- toIO $
      debugSession config $ title >> _test_session

    return $ case result of
      Right _ -> httpAssertionsToResult $ summarize assertions
      Left err -> TT.testFailed $
        "Unhandled error: " ++ printErr (const "") err

httpAssertionsToResult :: AssertionSummary -> TT.Result
httpAssertionsToResult x =
  if numFailures x > 0
    then TT.testFailed $ unlines $ map showAssertion $ failures x
    else TT.testPassed $ show (numSuccesses x) ++ " assertion(s)"

testCase
  :: (Effectful m, Typeable m)
  => TT.TestName
  -> HttpSession m () () () () ()
  -> TT.TestTree
testCase name test =
  testCaseWithSetup name (return ()) (return ()) test


testCaseWithSetup
  :: (Effectful m, Typeable m)
  => TT.TestName
  -> HttpSession m () () () () () -- ^ Setup
  -> HttpSession m () () () () () -- ^ Teardown
  -> HttpSession m () () () () () -- ^ The test
  -> TT.TestTree
testCaseWithSetup name setup teardown test =
  TT.singleTest name $ HttpSessionTest
    { _test_name = Just name
    , _test_session = setup >> test >> teardown
    }


-- | Log location.
newtype LogHandle
  = LogHandle { theLogHandle :: FileHandle }
  deriving Typeable

instance TO.IsOption LogHandle where
  defaultValue = LogHandle StdErr
  parseValue path = Just $ LogHandle $ Path path
  optionName = return "log file name"
  optionHelp = return "default: stderr"



-- | Assertion log location.
newtype AssertionLogHandle
  = AssertionLogHandle { theAssertionLogHandle :: FileHandle }
  deriving Typeable

instance TO.IsOption AssertionLogHandle where
  defaultValue = AssertionLogHandle StdOut
  parseValue path = Just $ AssertionLogHandle $ Path path
  optionName = return "assertion log file name"
  optionHelp = return "default: stdout"


-- | Representation of file handles, both paths and the stdin/out/err handles.
data FileHandle
  = StdIn
  | StdErr
  | StdOut
  | Path FilePath

writeModeHandle :: FileHandle -> IO Handle
writeModeHandle x = case x of
  StdIn -> error "writeModeHandle: Cannot open stdin in write mode."
  StdOut -> return stdout
  StdErr -> return stderr
  Path path -> openFile path WriteMode

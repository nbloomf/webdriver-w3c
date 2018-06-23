{-# LANGUAGE Rank2Types, OverloadedStrings #-}
module Web.Api.WebDriver.Assert.Test (
    tests
) where

import System.IO
import Data.String

import qualified Test.Tasty as TT (TestTree(), testGroup)
import qualified Test.Tasty.QuickCheck as QC (testProperty)
import qualified Test.Tasty.HUnit as HU

import Control.Concurrent (MVar)
import qualified Network.Wreq as Wreq
import qualified Data.Map as MS

import Data.MockIO
import Control.Monad.Script.Http
import Web.Api.WebDriver.Monad.Test.Server

import Web.Api.WebDriver
import Web.Api.WebDriver.Monad.Test.Server



tests :: MVar () -> TT.TestTree
tests lock = TT.testGroup "Web.Api.WebDriver.Assert"
  [ TT.testGroup "Mock"
    [ assertionTestCases (mockConfig lock) condMockIO
    ]
  , TT.testGroup "Real"
    [ assertionTestCases (realConfig lock) condIO
    ]
  ]



condIO
  :: IO (Either (E WDError) t, S WDState, W WDError WDLog)
  -> IO AssertionSummary
condIO x = do
  (_,_,w) <- x
  return $ summarize $ getAssertions $ logEntries w

realConfig :: MVar () -> WebDriverConfig IO
realConfig lock = WDConfig
  { _initialState = defaultWebDriverState
  , _environment = defaultWebDriverEnvironment
    { _logLock = Just lock
    , _logOptions = defaultWebDriverLogOptions
      { _logSilent = True
      }
    }
  , _evaluator = evalIO evalWDAct
  }



condMockIO
  :: MockIO WebDriverServerState (Either (E WDError) t, S WDState, W WDError WDLog)
  -> IO AssertionSummary
condMockIO x = do
  let ((_,_,w),_) = runMockIO x defaultWebDriverServer
  return $ summarize $ getAssertions $ logEntries w

mockConfig :: MVar () -> WebDriverConfig (MockIO WebDriverServerState)
mockConfig lock = WDConfig
  { _evaluator = evalMockIO evalWDActMockIO
  , _initialState = defaultWebDriverState
  , _environment = defaultWebDriverEnvironment
    { _logLock = Just lock
    , _logOptions = defaultWebDriverLogOptions
      { _logSilent = True
      }
    }
  }





assertionTestCases
  :: (Monad eff)
  => WebDriverConfig eff
  -> (eff (Either (E WDError) (), S WDState, W WDError WDLog) -> IO AssertionSummary)
  -> TT.TestTree
assertionTestCases config cond = TT.testGroup "Assertions"
  [ QC.testProperty "assertSuccess" $
    checkWebDriver config cond
      (== summarize [success "Success!" "yay!"]) $
      do
        assertSuccess "yay!"

  , QC.testProperty "assertFailure" $
    checkWebDriver config cond
      (== summarize [failure "Failure :(" "oh no"]) $
      do
        assertFailure "oh no"

  , QC.testProperty "assertTrue (success)" $ \msg ->
    checkWebDriver config cond
      (== summarize [success "True is True" msg]) $
      do
        assertTrue True msg

  , QC.testProperty "assertTrue (failure)" $ \msg ->
    checkWebDriver config cond
      (== summarize [failure "False is True" msg]) $
      do
        assertTrue False msg

  , QC.testProperty "assertFalse (success)" $ \msg ->
    checkWebDriver config cond
      (== summarize [success "False is False" msg]) $
      do
        assertFalse False msg

  , QC.testProperty "assertFalse (failure)" $ \msg ->
    checkWebDriver config cond
      (== summarize [failure "True is False" msg]) $
      do
        assertFalse True msg

  , QC.testProperty "assertEqual (Int, success)" $ \k ->
    checkWebDriver config cond
      (== summarize
        [success
          (fromString $ show k ++ " is equal to " ++ show k)
          (fromString $ show k)
        ]
      ) $
      do
        assertEqual (k :: Int) k (fromString $ show k)

  , QC.testProperty "assertEqual (Int, failure)" $ \k ->
    checkWebDriver config cond
      (== summarize
        [failure
          (fromString $ show (k+1) ++ " is equal to " ++ show k)
          (fromString $ show k)
        ]
      ) $
      do
        assertEqual (k+1 :: Int) k (fromString $ show k)

  , QC.testProperty "assertEqual (String, success)" $ \str ->
    checkWebDriver config cond
      (== summarize
        [success
          (fromString $ show str ++ " is equal to " ++ show str)
          (fromString str)
        ]
      ) $
      do
        assertEqual (str :: String) str (fromString str)

  , QC.testProperty "assertEqual (String, failure)" $ \str ->
    checkWebDriver config cond
      (== summarize
        [failure
          (fromString $ show (str++"?") ++ " is equal to " ++ show str)
          (fromString str)
        ]
      ) $
      do
        assertEqual (str++"?" :: String) str (fromString str)

  , QC.testProperty "assertNotEqual (Int, success)" $ \k ->
    checkWebDriver config cond
      (== summarize
        [success
          (fromString $ show (k+1) ++ " is not equal to " ++ show k)
          (fromString $ show k)
        ]
      ) $
      do
        assertNotEqual (k+1 :: Int) k (fromString $ show k)

  , QC.testProperty "assertNotEqual (Int, failure)" $ \k ->
    checkWebDriver config cond
      (== summarize
        [failure
          (fromString $ show k ++ " is not equal to " ++ show k)
          (fromString $ show k)
        ]
      ) $
      do
        assertNotEqual (k :: Int) k (fromString $ show k)

  , QC.testProperty "assertNotEqual (String, success)" $ \str ->
    checkWebDriver config cond
      (== summarize
        [success
          (fromString $ show (str++"?") ++ " is not equal to " ++ show str)
          (fromString str)
        ]
      ) $
      do
        assertNotEqual (str++"?" :: String) str (fromString str)

  , QC.testProperty "assertNotEqual (String, failure)" $ \str ->
    checkWebDriver config cond
      (== summarize
        [failure
          (fromString $ show str ++ " is not equal to " ++ show str)
          (fromString str)
        ]
      ) $
      do
        assertNotEqual (str :: String) str (fromString str)

    , QC.testProperty "assertIsSubstring (success)" $ \str1 str2 ->
    checkWebDriver config cond
      (== summarize
        [success
          (fromString $ show str1 ++ " is a substring of " ++ show (str2++str1++str2))
          (fromString str1)
        ]
      ) $
      do
        assertIsSubstring (str1 :: String) (str2++str1++str2) (fromString str1)

    , QC.testProperty "assertIsSubstring (failure)" $ \c str1 str2 ->
    let str3 = filter (/= c) str2 in
    checkWebDriver config cond
      (== summarize
        [failure
          (fromString $ show (c:str1) ++ " is a substring of " ++ show str3)
          (fromString str1)
        ]
      ) $
      do
        assertIsSubstring (c:str1 :: String) (str3) (fromString str1)

    , QC.testProperty "assertIsNotSubstring (success)" $ \c str1 str2 ->
    let str3 = filter (/= c) str2 in
    checkWebDriver config cond
      (== summarize
        [success
          (fromString $ show (c:str1) ++ " is not a substring of " ++ show str3)
          (fromString str1)
        ]
      ) $
      do
        assertIsNotSubstring (c:str1 :: String) (str3) (fromString str1)

    , QC.testProperty "assertIsNotSubstring (failure)" $ \str1 str2 ->
    checkWebDriver config cond
      (== summarize
        [failure
          (fromString $ show str1 ++ " is not a substring of " ++ show (str2++str1++str2))
          (fromString str1)
        ]
      ) $
      do
        assertIsNotSubstring (str1 :: String) (str2++str1++str2) (fromString str1)

    , QC.testProperty "assertIsNamedSubstring (success)" $ \name str1 str2 ->
    checkWebDriver config cond
      (== summarize
        [success
          (fromString $ show str1 ++ " is a substring of " ++ name)
          (fromString str1)
        ]
      ) $
      do
        assertIsNamedSubstring (str1 :: String) (str2++str1++str2, name) (fromString str1)

    , QC.testProperty "assertIsNamedSubstring (failure)" $ \name c str1 str2 ->
    let str3 = filter (/= c) str2 in
    checkWebDriver config cond
      (== summarize
        [failure
          (fromString $ show (c:str1) ++ " is a substring of " ++ name)
          (fromString str1)
        ]
      ) $
      do
        assertIsNamedSubstring (c:str1 :: String) (str3,name) (fromString str1)

    , QC.testProperty "assertIsNotNamedSubstring (success)" $ \name c str1 str2 ->
    let str3 = filter (/= c) str2 in
    checkWebDriver config cond
      (== summarize
        [success
          (fromString $ show (c:str1) ++ " is not a substring of " ++ name)
          (fromString str1)
        ]
      ) $
      do
        assertIsNotNamedSubstring (c:str1 :: String) (str3,name) (fromString str1)

    , QC.testProperty "assertIsNotNamedSubstring (failure)" $ \name str1 str2 ->
    checkWebDriver config cond
      (== summarize
        [failure
          (fromString $ show str1 ++ " is not a substring of " ++ name)
          (fromString str1)
        ]
      ) $
      do
        assertIsNotNamedSubstring (str1 :: String) (str2++str1++str2, name) (fromString str1)
  ]

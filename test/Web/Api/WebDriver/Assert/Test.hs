{-# LANGUAGE Rank2Types, OverloadedStrings #-}
module Web.Api.WebDriver.Assert.Test (
    tests
) where

import System.IO
import Data.String

import qualified Test.Tasty as TT (TestTree(), testGroup)
import qualified Test.Tasty.QuickCheck as QC (testProperty)
import qualified Test.Tasty.HUnit as HU
import Test.QuickCheck (Arbitrary(..))

import Data.Text (Text)
import qualified Data.Text as T

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

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary



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
    checkWebDriverT config cond
      (== summarize [success "Success!" "yay!"]) $
      do
        assertSuccess "yay!"

  , QC.testProperty "assertFailure" $
    checkWebDriverT config cond
      (== summarize [failure "Failure :(" "oh no"]) $
      do
        assertFailure "oh no"

  , QC.testProperty "assertTrue (success)" $ \msg ->
    checkWebDriverT config cond
      (== summarize [success "True is True" msg]) $
      do
        assertTrue True msg

  , QC.testProperty "assertTrue (failure)" $ \msg ->
    checkWebDriverT config cond
      (== summarize [failure "False is True" msg]) $
      do
        assertTrue False msg

  , QC.testProperty "assertFalse (success)" $ \msg ->
    checkWebDriverT config cond
      (== summarize [success "False is False" msg]) $
      do
        assertFalse False msg

  , QC.testProperty "assertFalse (failure)" $ \msg ->
    checkWebDriverT config cond
      (== summarize [failure "True is False" msg]) $
      do
        assertFalse True msg

  , QC.testProperty "assertEqual (Int, success)" $ \k ->
    checkWebDriverT config cond
      (== summarize
        [success
          (AssertionStatement $
            T.pack (show k) <> " is equal to " <> T.pack (show k))
          (AssertionComment $ T.pack $ show k)
        ]
      ) $
      do
        assertEqual
          (k :: Int) k
          (AssertionComment $ T.pack $ show k)

  , QC.testProperty "assertEqual (Int, failure)" $ \k ->
    checkWebDriverT config cond
      (== summarize
        [failure
          (AssertionStatement $
            T.pack (show (k+1)) <> " is equal to " <> T.pack (show k))
          (AssertionComment $ T.pack $ show k)
        ]
      ) $
      do
        assertEqual
          (k+1 :: Int) k
          (AssertionComment $ T.pack $ show k)

  , QC.testProperty "assertEqual (Text, success)" $ \str ->
    checkWebDriverT config cond
      (== summarize
        [success
          (AssertionStatement $
            T.pack (show str) <> " is equal to " <> T.pack (show str))
          (AssertionComment str)
        ]
      ) $
      do
        assertEqual
          (str :: Text) str
          (AssertionComment str)

  , QC.testProperty "assertEqual (Text, failure)" $ \str ->
    checkWebDriverT config cond
      (== summarize
        [failure
          (AssertionStatement $
            T.pack (show $ str <> "?") <> " is equal to " <> T.pack (show str))
          (AssertionComment str)
        ]
      ) $
      do
        assertEqual
          (str <> "?" :: Text) str
          (AssertionComment str)

  , QC.testProperty "assertNotEqual (Int, success)" $ \k ->
    checkWebDriverT config cond
      (== summarize
        [success
          (AssertionStatement $
            T.pack (show (k+1)) <> " is not equal to " <> T.pack (show k))
          (AssertionComment $ T.pack $ show k)
        ]
      ) $
      do
        assertNotEqual
          (k+1 :: Int) k
          (AssertionComment $ T.pack $ show k)

  , QC.testProperty "assertNotEqual (Int, failure)" $ \k ->
    checkWebDriverT config cond
      (== summarize
        [failure
          (AssertionStatement $
            T.pack (show k) <> " is not equal to " <> T.pack (show k))
          (AssertionComment $ T.pack $ show k)
        ]
      ) $
      do
        assertNotEqual
          (k :: Int) k
          (AssertionComment $ T.pack $ show k)

  , QC.testProperty "assertNotEqual (Text, success)" $ \str ->
    checkWebDriverT config cond
      (== summarize
        [success
          (AssertionStatement $
            T.pack (show (str <> "?")) <> " is not equal to " <> T.pack (show str))
          (AssertionComment str)
        ]
      ) $
      do
        assertNotEqual
          (str <> "?" :: Text)
          (str)
          (AssertionComment str)

  , QC.testProperty "assertNotEqual (Text, failure)" $ \str ->
    checkWebDriverT config cond
      (== summarize
        [failure
          (AssertionStatement $
            T.pack (show str) <> " is not equal to " <> T.pack (show str))
          (AssertionComment str)
        ]
      ) $
      do
        assertNotEqual
          (str :: Text)
          (str)
          (AssertionComment str)

    , QC.testProperty "assertIsSubstring (success)" $ \str1 str2 ->
    checkWebDriverT config cond
      (== summarize
        [success
          (AssertionStatement $
            T.pack (show str1) <> " is a substring of " <> T.pack (show $ str2 <> str1 <> str2))
          (AssertionComment str1)
        ]
      ) $
      do
        assertIsSubstring
          (str1 :: Text)
          (str2 <> str1 <> str2)
          (AssertionComment str1)

    , QC.testProperty "assertIsSubstring (failure)" $ \c str1 str2 ->
    let str3 = T.filter (/= c) str2 in
    checkWebDriverT config cond
      (== summarize
        [failure
          (AssertionStatement $
            T.pack (show $ T.cons c str1) <> " is a substring of " <> T.pack (show str3))
          (AssertionComment str1)
        ]
      ) $
      do
        assertIsSubstring
          (T.cons c str1 :: Text)
          (str3)
          (AssertionComment str1)

    , QC.testProperty "assertIsNotSubstring (success)" $ \c str1 str2 ->
    let str3 = T.filter (/= c) str2 in
    checkWebDriverT config cond
      (== summarize
        [success
          (AssertionStatement $
            T.pack (show $ T.cons c str1) <> " is not a substring of " <> T.pack (show str3))
          (AssertionComment str1)
        ]
      ) $
      do
        assertIsNotSubstring
          (T.cons c str1 :: Text)
          (str3)
          (AssertionComment str1)

    , QC.testProperty "assertIsNotSubstring (failure)" $ \str1 str2 ->
    checkWebDriverT config cond
      (== summarize
        [failure
          (AssertionStatement $
            T.pack (show str1) <> " is not a substring of " <> T.pack (show $ str2 <> str1 <> str2))
          (AssertionComment str1)
        ]
      ) $
      do
        assertIsNotSubstring
          (str1 :: Text)
          (str2 <> str1 <> str2)
          (AssertionComment str1)

    , QC.testProperty "assertIsNamedSubstring (success)" $ \name str1 str2 ->
    checkWebDriverT config cond
      (== summarize
        [success
          (AssertionStatement $
            T.pack (show str1) <> " is a substring of " <> name)
          (AssertionComment str1)
        ]
      ) $
      do
        assertIsNamedSubstring
          (str1 :: Text)
          (str2 <> str1 <> str2, name)
          (AssertionComment str1)

    , QC.testProperty "assertIsNamedSubstring (failure)" $ \name c str1 str2 ->
    let str3 = T.filter (/= c) str2 in
    checkWebDriverT config cond
      (== summarize
        [failure
          (AssertionStatement $
            T.pack (show $ T.cons c str1) <> " is a substring of " <> name)
          (AssertionComment str1)
        ]
      ) $
      do
        assertIsNamedSubstring
          (T.cons c str1 :: Text)
          (str3, name)
          (AssertionComment str1)

    , QC.testProperty "assertIsNotNamedSubstring (success)" $ \name c str1 str2 ->
    let str3 = T.filter (/= c) str2 in
    checkWebDriverT config cond
      (== summarize
        [success
          (AssertionStatement $
            T.pack (show $ T.cons c str1) <> " is not a substring of " <> name)
          (AssertionComment str1)
        ]
      ) $
      do
        assertIsNotNamedSubstring
          (T.cons c str1 :: Text)
          (str3, name)
          (AssertionComment str1)

    , QC.testProperty "assertIsNotNamedSubstring (failure)" $ \name str1 str2 ->
    checkWebDriverT config cond
      (== summarize
        [failure
          (AssertionStatement $
            T.pack (show str1) <> " is not a substring of " <> name)
          (AssertionComment str1)
        ]
      ) $
      do
        assertIsNotNamedSubstring
          (str1 :: Text)
          (str2 <> str1 <> str2, name)
          (AssertionComment str1)
  ]

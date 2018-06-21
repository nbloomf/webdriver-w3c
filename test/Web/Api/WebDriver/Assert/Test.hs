{-# LANGUAGE Rank2Types, OverloadedStrings #-}
module Web.Api.WebDriver.Assert.Test (
    tests
) where

import System.IO

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
    [ checkAssertionCases (testMock lock id)
    , checkAssertionSummaryCases (testMock lock summarize)
    ]
  , TT.testGroup "Real"
    [ checkAssertionCases (testReal lock id)
    , checkAssertionSummaryCases (testReal lock summarize)
    ]
  ]



defaultState :: S WDState
defaultState = S
  { _httpOptions = Wreq.defaults
  , _httpSession = Nothing
  , _userState = WDState
    { _sessionId = Nothing
    }
  }

defaultEnv :: MVar () -> R WDError WDLog WDEnv
defaultEnv lock = R
  { _logHandle = stdout
  , _logLock = Just lock
  , _uid = ""
  , _logOptions = trivialLogOptions
    { _logColor = True
    , _logJson = True
    , _logHeaders = False
    , _logSilent = True
    , _printUserError = printWDError
    , _printUserLog = printWDLog
    }
  , _httpErrorInject = promoteHttpResponseError
  , _env = WDEnv
    { _remoteHostname = "localhost"
    , _remotePort = 4444
    , _remotePath = ""
    , _responseFormat = SpecFormat
    , _apiVersion = CR_2018_03_04
    , _secretsPath = ""
    , _vars = MS.fromList []
    , _stdout = stdout
    , _stdin = stdin
    }
  }

realConfig :: MVar () -> WebDriverConfig IO
realConfig lock = WDConfig
  { _initialState = defaultState
  , _environment = defaultEnv lock
  , _evaluator = evalIO evalWDAct
  }

mockConfig :: MVar () -> WebDriverConfig (MockIO WebDriverServerState)
mockConfig lock = WDConfig
  { _evaluator = evalMockIO evalWDActMockIO
  , _initialState = defaultState
  , _environment = defaultEnv lock
  }



testReal
  :: (Show t, Eq t)
  => MVar ()
  -> ([Assertion] -> t)
  -> (String, WebDriver (), t)
  -> TT.TestTree
testReal lock f =
  checkCase (realConfig lock) id f

testMock
  :: (Show t, Eq t)
  => MVar ()
  -> ([Assertion] -> t)
  -> (String, WebDriver (), t)
  -> TT.TestTree
testMock lock f =
  checkCase (mockConfig lock)
  (\x -> return $ fst $ runMockIO x defaultWebDriverServer) f

checkCase
  :: (Monad m, Show t, Eq t)
  => WebDriverConfig m
  -> (forall a. m a -> IO a)
  -> ([Assertion] -> t)
  -> (String, WebDriver (), t)
  -> TT.TestTree
checkCase config eval f (name,http,expect) =
  HU.testCase name $ do
    (_,_,w) <- eval $ execWebDriver config http
    let actual = f $ getAssertions $ logEntries w
    if expect == actual
      then return ()
      else HU.assertFailure $
        "\n\ngot:\n " ++ show actual ++ "\n\nbut expected:\n" ++ show expect



checkAssertionCases
  :: ((String, WebDriver (), [Assertion]) -> TT.TestTree)
  -> TT.TestTree
checkAssertionCases check =
  TT.testGroup "assertions" $ map check _assertionCases

checkAssertionSummaryCases
  :: ((String, WebDriver (), AssertionSummary) -> TT.TestTree)
  -> TT.TestTree
checkAssertionSummaryCases check =
  TT.testGroup "assertions" $ map check _assertionSummaryCases



_assertionCases :: [(String, WebDriver (), [Assertion])]
_assertionCases =
  [ ( "assertSuccess"
    , assertSuccess "yay!"
    , [success "Success!" "yay!"]
    )

  , ( "assertFailure"
    , assertFailure "oh no"
    , [failure "Failure :(" "oh no"]
    )

  , ( "assertTrue (success)"
    , assertTrue True "test"
    , [success "True is True" "test"]
    )

  , ( "assertTrue (failure)"
    , assertTrue False "test"
    , [failure "False is True" "test"]
    )

  , ( "assertFalse (success)"
    , assertFalse False "test"
    , [success "False is False" "test"]
    )

  , ( "assertFalse (failure)"
    , assertFalse True "test"
    , [failure "True is False" "test"]
    )

  , ( "assertEqual (Int, success)"
    , assertEqual (1::Int) (1::Int) "test"
    , [success "1 is equal to 1" "test"]
    )

  , ( "assertEqual (Int, failure)"
    , assertEqual (2::Int) (1::Int) "test"
    , [failure "2 is equal to 1" "test"]
    )

  , ( "assertEqual (String, success)"
    , assertEqual "A" "A" "test"
    , [success "\"A\" is equal to \"A\"" "test"]
    )

  , ( "assertEqual (String, failure)"
    , assertEqual "B" "A" "test"
    , [failure "\"B\" is equal to \"A\"" "test"]
    )

  , ( "assertNotEqual (Int, success)"
    , assertNotEqual (2::Int) (1::Int) "test"
    , [success "2 is not equal to 1" "test"]
    )

  , ( "assertNotEqual (Int, failure)"
    , assertNotEqual (1::Int) (1::Int) "test"
    , [failure "1 is not equal to 1" "test"]
    )

  , ( "assertNotEqual (String, success)"
    , assertNotEqual "B" "A" "test"
    , [success "\"B\" is not equal to \"A\"" "test"]
    )

  , ( "assertNotEqual (String, failure)"
    , assertNotEqual "A" "A" "test"
    , [failure "\"A\" is not equal to \"A\"" "test"]
    )

  , ( "assertIsSubstring (success)"
    , assertIsSubstring "oba" "foobar" "test"
    , [success "\"oba\" is a substring of \"foobar\"" "test"]
    )

  , ( "assertIsSubstring (failure)"
    , assertIsSubstring "quux" "foobar" "test"
    , [failure "\"quux\" is a substring of \"foobar\"" "test"]
    )

  , ( "assertIsNotSubstring (success)"
    , assertIsNotSubstring "quux" "foobar" "test"
    , [success "\"quux\" is not a substring of \"foobar\"" "test"]
    )

  , ( "assertIsNotSubstring (failure)"
    , assertIsNotSubstring "oba" "foobar" "test"
    , [failure "\"oba\" is not a substring of \"foobar\"" "test"]
    )

  , ( "assertIsNamedSubstring (success)"
    , assertIsNamedSubstring "oba" ("foobar","string") "test"
    , [success "\"oba\" is a substring of string" "test"]
    )

  , ( "assertIsNamedSubstring (failure)"
    , assertIsNamedSubstring "quux" ("foobar","string") "test"
    , [failure "\"quux\" is a substring of string" "test"]
    )

  , ( "assertIsNotNamedSubstring (success)"
    , assertIsNotNamedSubstring "quux" ("foobar","string") "test"
    , [success "\"quux\" is not a substring of string" "test"]
    )

  , ( "assertIsNotNamedSubstring (failure)"
    , assertIsNotNamedSubstring "oba" ("foobar","string") "test"
    , [failure "\"oba\" is not a substring of string" "test"]
    )
  ]



_assertionSummaryCases :: [(String, WebDriver (), AssertionSummary)]
_assertionSummaryCases =
  [ ( "assertSuccess"
    , assertSuccess "yay!"
    , summarize [success "Success!" "yay!"]
    )

  , ( "assertFailure"
    , assertFailure "oh no"
    , summarize [failure "Failure :(" "oh no"]
    )
  ]

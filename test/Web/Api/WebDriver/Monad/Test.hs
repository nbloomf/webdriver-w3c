{-# LANGUAGE BangPatterns, GADTs #-}
module Web.Api.WebDriver.Monad.Test (
    tests
) where

import Test.Tasty (TestTree(), testGroup, localOption)

import Data.MockIO

import Web.Api.WebDriver
import Test.Tasty.WebDriver

import Web.Api.WebDriver.Monad.Test.Server
import Web.Api.WebDriver.Monad.Test.Session.Success
import Web.Api.WebDriver.Monad.Test.Session.UnknownError
import Web.Api.WebDriver.Monad.Test.Session.InvalidElementState


tests :: FilePath -> TestTree
tests path = testGroup "Web.Api.WebDriver.Monad"
  [   localOption (ApiResponseFormat SpecFormat)
    $ localOption (SilentLog)
    $ testGroup "Mock Driver" $ endpointTests testCaseMockIO path

  ,   localOption (Driver Geckodriver)
    $ localOption (ApiResponseFormat SpecFormat)
    $ localOption (Headless True)
    $ localOption (SilentLog)
    $ testGroup "Geckodriver" $ endpointTests testCase path

  ,   localOption (Driver Chromedriver)
    $ localOption (ApiResponseFormat SpecFormat)
    $ localOption (Headless True)
    $ ifTierIs TEST (localOption (BrowserPath $ Just "/usr/bin/google-chrome"))
    $ localOption (SilentLog)
    $ testGroup "Chromedriver" $ endpointTests testCase path
  ]



testCaseMockIO :: String -> WebDriverT (MockIO WebDriverServerState) () -> TestTree
testCaseMockIO name = testCaseM name
  (evalMockIO evalWDActMockIO)
  (\x -> return $ fst $ runMockIO x defaultWebDriverServer)

endpointTests
  :: (Monad eff)
  => (String -> WebDriverT eff () -> TestTree)
  -> FilePath
  -> [TestTree]
endpointTests buildTestCase path =
  [ successfulExit buildTestCase path
  , invalidElementStateExit buildTestCase path
  , unknownErrorExit buildTestCase path
  ]

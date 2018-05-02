{-# LANGUAGE BangPatterns #-}
module Web.Api.WebDriver.Monad.Test (
    tests
  ) where

import Data.Typeable
  ( Typeable )
import System.IO

import Test.Tasty (TestTree(), testGroup, localOption)

import Web.Api.Http hiding (TestTree)
import Web.Api.WebDriver
import Test.Tasty.WebDriver

import Web.Api.Http.Effects.Test.Mock
import Web.Api.WebDriver.Monad.Test.Server
import Web.Api.WebDriver.Monad.Test.Session.Success
import Web.Api.WebDriver.Monad.Test.Session.UnknownError
import Web.Api.WebDriver.Monad.Test.Session.InvalidElementState


tests :: FilePath -> TestTree
tests path = testGroup "Web.Api.WebDriver.Monad"
  [ testGroup "Mock Driver"
      (endpointTests path (return () :: MockIO WebDriverServerState ()))

  ,   localOption (Driver Geckodriver)
    $ localOption (ApiResponseFormat SpecFormat)
    $ localOption (RemotePort 4444)
    $ localOption (LogHandle $ Path "/dev/null")
    $ localOption (AssertionLogHandle $ Path "/dev/null")
    $ testGroup "Geckodriver" (endpointTests path (return () :: IO ()))

  ,   localOption (Driver Chromedriver)
    $ localOption (ApiResponseFormat ChromeFormat)
    $ localOption (RemotePort 9515)
    $ localOption (LogHandle $ Path "/dev/null")
    $ localOption (AssertionLogHandle $ Path "/dev/null")
    $ testGroup "Chromedriver" (endpointTests path (return () :: IO ()))
  ]


endpointTests :: (Effectful m, Typeable m) => FilePath -> m () -> [TestTree]
endpointTests path x =
  [ successfulExit path x
  , invalidElementStateExit path x
  , unknownErrorExit path x
  ]

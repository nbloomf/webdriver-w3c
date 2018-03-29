{-# LANGUAGE BangPatterns #-}
module Web.Api.WebDriver.Monad.Test (
    tests
  ) where

import Data.Time.Clock.System
  ( getSystemTime, systemToUTCTime )
import Data.Typeable
  ( Typeable )
import qualified Network.Wreq.Session as WreqS
  ( newSession )
import System.IO

import Test.Tasty (TestTree(), testGroup, localOption)

import Web.Api.Http hiding (TestTree)
import Web.Api.WebDriver
import Test.Tasty.WebDriver

import Web.Api.Http.Effects.Test.Mock
import Web.Api.WebDriver.Monad.Test.Server
import Web.Api.WebDriver.Monad.Test.Session.Success
import Web.Api.WebDriver.Monad.Test.Session.UnknownError


tests :: TestTree
tests = testGroup "Web.Api.WebDriver.Monad"
  [ testGroup "Mock Server"
      (endpointTests (return () :: MockIO WebDriverServerState ()))

  ,   localOption (LogHandle $ Path "/dev/null")
    $ localOption (AssertionLogHandle $ Path "/dev/null")
    $ testGroup "Real Server" (endpointTests (return () :: IO ()))
  ]


endpointTests :: (Effectful m, Typeable m) => m () -> [TestTree]
endpointTests x =
  [ successfulExit x
  , unknownErrorExit x
  ]

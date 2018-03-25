{-# LANGUAGE BangPatterns #-}
module Web.Api.WebDriver.Monad.Test (
    tests
  ) where

import Data.Time.Clock.System
  ( getSystemTime, systemToUTCTime )
import qualified Network.Wreq.Session as WreqS
  ( newSession )
import System.IO

import Test.Tasty (TestTree(), testGroup)
import Test.Tasty.QuickCheck as QC (testProperty)
import Test.Tasty.HUnit as HU

import Web.Api.Http hiding (TestTree)
import Web.Api.WebDriver

import Web.Api.Http.Effects.Test.Mock
import Web.Api.WebDriver.Monad.Test.Server
import Web.Api.WebDriver.Monad.Test.Sessions

tests :: TestTree
tests = testGroup "Web.Api.WebDriver.Monad"
  [ mock_test_exit_success "mock geckodriver defaults"
      defaultWebDriverConfig
      defaultFirefoxCapabilities

  , real_test_exit_success "real geckodriver defaults"
      (\h -> setEnv (setLogHandle h) defaultWebDriverConfig)
      defaultFirefoxCapabilities

  , mock_test_unknown_error "mock geckodriver defaults"
      defaultWebDriverConfig
      defaultFirefoxCapabilities

  , real_test_unknown_error "real geckodriver defaults"
      (\h -> setEnv (setLogHandle h) defaultWebDriverConfig)
      defaultFirefoxCapabilities
  ]



-- | Prepare initial state for MockSt.
init_state :: ([String],String) -> IO (MockSt WebDriverServerState)
init_state stdin = do
  time <- fmap systemToUTCTime getSystemTime
  session <- WreqS.newSession
  return $ mockSt time defaultWebDriverServer
    session stdin defaultWebDriverServerState



{----------------}
{- Exit Success -}
{----------------}

-- | Sessions which should not error out.
mock_exit_success
  :: WebDriverConfig
  -> Capabilities
  -> WebDriver (MockIO WebDriverServerState) ()
  -> IO ()
mock_exit_success config caps session = do
  state <- init_state ([],"")
  let
    (result, st) = runMockIO
      (runSession config $ runIsolated caps session) state
  case result of
    Right () -> return ()
    Left err -> HU.assertFailure $ show err

mock_test_exit_success
  :: String
  -> WebDriverConfig
  -> Capabilities
  -> TestTree
mock_test_exit_success name config caps = testGroup ("exit success: " ++ name) $
  map
    (\(title, session) -> HU.testCase title $ mock_exit_success config caps session)
    _exit_success_cases



real_exit_success
  :: (Handle -> WebDriverConfig)
  -> Capabilities
  -> WebDriver IO ()
  -> IO ()
real_exit_success config caps session = do
  h <- openFile "/dev/null" WriteMode
  result <- runSession (config h) $ runIsolated caps session
  case result of
    Right () -> return ()
    Left err -> HU.assertFailure $ show err

real_test_exit_success
  :: String
  -> (Handle -> WebDriverConfig)
  -> Capabilities
  -> TestTree
real_test_exit_success name config caps = testGroup ("exit success: " ++ name) $
  map
    (\(title, session) -> HU.testCase title $ real_exit_success config caps session)
    _exit_success_cases



{-----------------}
{- Unknown Error -}
{-----------------}

-- | Sessions which should not error out.
mock_unknown_error
  :: WebDriverConfig
  -> Capabilities
  -> WebDriver (MockIO WebDriverServerState) ()
  -> IO ()
mock_unknown_error config caps session = do
  state <- init_state ([],"")
  let
    (result, st) = runMockIO
      (runSession config $ runIsolated caps session) state
  case result of
    Right () -> HU.assertFailure "unexpected success"
    Left err -> case err of
      Err (ResponseError UnknownError _ _ _ _) -> return ()
      _ -> HU.assertFailure $ show err

mock_test_unknown_error
  :: String
  -> WebDriverConfig
  -> Capabilities
  -> TestTree
mock_test_unknown_error name config caps = testGroup ("unknown error: " ++ name) $
  map
    (\(title, session) -> HU.testCase title $ mock_unknown_error config caps session)
    _unknown_error_cases



real_unknown_error
  :: (Handle -> WebDriverConfig)
  -> Capabilities
  -> WebDriver IO ()
  -> IO ()
real_unknown_error config caps session = do
  h <- openFile "/dev/null" WriteMode
  result <- runSession (config h) $ runIsolated caps session
  case result of
    Right () -> HU.assertFailure "unexpected success"
    Left err -> case err of
      Err (ResponseError UnknownError _ _ _ _) -> return ()
      _ -> HU.assertFailure $ show err

real_test_unknown_error
  :: String
  -> (Handle -> WebDriverConfig)
  -> Capabilities
  -> TestTree
real_test_unknown_error name config caps = testGroup ("unknown error: " ++ name) $
  map
    (\(title, session) -> HU.testCase title $ real_unknown_error config caps session)
    _unknown_error_cases

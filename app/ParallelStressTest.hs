{-
This program is meant to simulate running a large number of tests in parallel. To set the number of tests to be run, export the WD_STRESS_TEST_NUM_TESTS variable in the shell.
-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.WebDriver

import Web.Api.WebDriver

import System.Exit (exitFailure)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

_test :: (Monad eff) => WebDriverT eff ()
_test = navigateTo "https://google.com"


main :: IO ()
main = do
  var <- lookupEnv "WD_STRESS_TEST_NUM_TESTS"

  k <- case var >>= readMaybe of
    Nothing -> showError
    Just m -> if m <= 0
      then showError
      else return m 

  defaultWebDriverMain $
    localOption (SilentLog) $
    testGroup "Test Demo" $
      [ testCase ("navigate to google.com #" ++ show i) _test | i <- [1..k] ]


showError :: IO a
showError = do
  putStrLn "WD_STRESS_TEST_NUM_TESTS must be a positive integer."
  exitFailure

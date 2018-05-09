module Main where

import System.Environment (setEnv, lookupEnv)
import System.Directory (getCurrentDirectory)

import Test.Tasty
import Test.Tasty.WebDriver

import Web.Api.Http.Assert.Test
import Web.Api.Http.Effects.Test
import Web.Api.WebDriver.Monad.Test
import Web.Api.WebDriver.Types.Test

main :: IO ()
main = do

  putStrLn ""
  putStrLn "\x1b[1;34m      ___  __   __   __          ___  __           __   __     ___  ___  __  ___ \x1b[0;39;49m"
  putStrLn "\x1b[1;34m|  | |__  |__) |  \\ |__) | \\  / |__  |__) __ |  | '__\\ /  ` __  |  |__  /__`  |  \x1b[0;39;49m"
  putStrLn "\x1b[1;34m|/\\| |___ |__) |__/ |  \\ |  \\/  |___ |  \\    |/\\| .__/ \\__,     |  |___ .__/  |  \x1b[0;39;49m"
  putStrLn "\x1b[1;34m                                                                                 \x1b[0;39;49m"

  setEnv "TASTY_NUM_THREADS" "1" -- needed for live tests
  testPagePath <- fmap (\path -> path ++ "/test/page") getCurrentDirectory

  deploy <- determineDeploymentTier

  defaultMain $ localOption (Deployment deploy) $ testGroup "All Tests"
    [ Web.Api.Http.Assert.Test.tests
    , Web.Api.WebDriver.Types.Test.tests
    , Web.Api.Http.Effects.Test.tests testPagePath
    , Web.Api.WebDriver.Monad.Test.tests ("file://" ++ testPagePath)
    ]

determineDeploymentTier :: IO DeploymentTier
determineDeploymentTier = do
  putStrLn "Determining deployment environment..."
  deploy <- do
    var <- lookupEnv "CI"
    case var of
      Just "true" -> return TEST
      _ -> return DEV
  putStrLn $ "Deployment environment is " ++ show deploy
  return deploy

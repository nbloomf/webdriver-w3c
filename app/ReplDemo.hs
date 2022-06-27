{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Data.List (isInfixOf)
import Data.Text (Text)

import Web.Api.WebDriver

{-
Utilities for demoing webdriver sessions. I mostly use this for testing
and development. To run an individual demo, first make sure your remote end
(chromedriver or geckodriver) is running on the correct port, then from ghci
say something like:

    withChromedriver normalChrome demoNewWindow

The executable runs all the demos, although this is less useful.
-}

main :: IO ()
main = do
  withChromedriver normalChrome demoNewWindow
  withChromedriver normalChrome demoGetComputedRole
  withChromedriver normalChrome demoGetComputedLabel

  withChromedriver headlessChrome demoPrintPage





withChromedriver :: Capabilities -> WebDriverT IO () -> IO ()
withChromedriver caps acts = do
  execWebDriverT chromeConfig $
    runIsolated_ caps acts
  return ()

chromeConfig :: WebDriverConfig IO
chromeConfig = defaultWebDriverConfig
  { _environment = defaultWebDriverEnvironment
    { _env = defaultWDEnv
      { _remotePort = 9515
      , _responseFormat = SpecFormat
      }
    }
  }

normalChrome :: Capabilities
normalChrome = defaultChromeCapabilities

headlessChrome :: Capabilities
headlessChrome = defaultChromeCapabilities
  { _chromeOptions = Just $ defaultChromeOptions
    { _chromeArgs = Just ["--headless"]
    }
  }

consoleLogChrome :: Capabilities
consoleLogChrome = defaultChromeCapabilities
  { _chromeOptions = Just $ defaultChromeOptions
    { _chromeArgs = Just ["--user-data-dir=datadir", "--enable-logging", "--v=1"]
    }
  }



demoNewWindow :: WebDriverT IO ()
demoNewWindow = do
  -- open google.com in the current tab
  navigateTo "https://www.google.com"

  -- open a new tab; but do not switch to it
  (handle, _) <- newWindow TabContext

  -- switch to the new tab
  switchToWindow handle

  -- open bing.com in this tab
  navigateTo "https://www.bing.com"

  wait 5000000
  return ()



demoGetComputedRole :: WebDriverT IO ()
demoGetComputedRole = do
  -- open google.com
  navigateTo "https://www.google.com"

  -- get the ARIA role of whatever element is active on page load
  role <- getActiveElement >>= getComputedRole

  comment $ "Computed role is '" <> role <> "'"
  wait 5000000
  return ()



demoGetComputedLabel :: WebDriverT IO ()
demoGetComputedLabel = do
  -- open google.com
  navigateTo "https://www.google.com"

  -- get the ARIA label of whatever element is active on page load
  role <- getActiveElement >>= getComputedLabel

  comment $ "Computed label is '" <> role <> "'"
  wait 5000000
  return ()



demoPrintPage :: WebDriverT IO ()
demoPrintPage = do
  -- open google.com
  navigateTo "https://www.google.com"

  -- print
  pdf <- printPage defaultPrintOptions
  writeBase64EncodedPdf "testprint.pdf" pdf

  wait 5000000
  return ()



-- use this to demonstrate getting the JS console log
--   withChromedriver consoleLogChrome demoConsoleLogChrome
demoConsoleLogChrome :: WebDriverT IO ()
demoConsoleLogChrome = do
  -- open google.com
  navigateTo "https://www.google.com"

  executeScript "console.error('HEYOOOO')" []

  -- logLines <- fmap lines $ liftIO $ readFile "~/datadir/chrome_debug.log" -- you'll need to expand ~ here
  -- let lines = filter ("CONSOLE" `isInfixOf`) logLines
  -- liftIO $ print lines

  wait 5000000
  return ()

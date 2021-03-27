module Main where

import Web.Api.WebDriver

{-
Utilities for demoing webdriver sessions. I mostly use this for testing
and development. To run an individual demo, first make sure your remote end
(chromedriver or geckodriver) is running on the correct port, then from ghci
say something like:

    withChromedriver demoNewWindow

The executable runs all the demos, although this is less useful.
-}

main :: IO ()
main = do
  withChromedriver demoNewWindow





withChromedriver :: WebDriverT IO () -> IO ()
withChromedriver acts = do
  execWebDriverT chromeConfig $
    runIsolated_ defaultChromeCapabilities acts
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

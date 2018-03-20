{- |
Module      : Web.Api.WebDriver.Types.Keyboard
Description : Type representing key presses.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

module Web.Api.WebDriver.Types.Keyboard (
    Key(..)
  , keyToChar
  ) where

-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#keyboard-actions>.
data Key
  = TabKey
  | EnterKey

-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#keyboard-actions>.
keyToChar :: Key -> Char
keyToChar key = case key of
  TabKey   -> '\xe004'
  EnterKey -> '\xe007'

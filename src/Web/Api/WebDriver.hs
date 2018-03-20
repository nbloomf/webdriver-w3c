{- |
Module      : Web.Api.WebDriver
Description : A monad for expressing WebDriver interactions.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

module Web.Api.WebDriver (
    module Web.Api.WebDriver.Endpoints
  , module Web.Api.WebDriver.Helpers
  , module Web.Api.WebDriver.Monad
  , module Web.Api.WebDriver.Types
  , module Web.Api.WebDriver.Types.Keyboard
  ) where

import Web.Api.WebDriver.Endpoints
import Web.Api.WebDriver.Helpers
import Web.Api.WebDriver.Monad
import Web.Api.WebDriver.Types
import Web.Api.WebDriver.Types.Keyboard

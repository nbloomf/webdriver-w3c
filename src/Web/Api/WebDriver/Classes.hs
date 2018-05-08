{- |
Module      : Web.Api.WebDriver.Classes
Description : Utility typeclasses
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

module Web.Api.WebDriver.Classes (
    HasElementRef(..)
  , HasContextId(..)
  ) where

import Web.Api.WebDriver.Types

-- | Types which carry a *web element reference* as described in https://www.w3.org/TR/webdriver/#elements.
class HasElementRef t where
  elementRefOf :: t -> ElementRef

instance HasElementRef ElementRef where
  elementRefOf = id


-- | Types which carry a *window handle* as described in https://www.w3.org/TR/webdriver/#command-contexts.
class HasContextId t where
  contextIdOf :: t -> ContextId

instance HasContextId ContextId where
  contextIdOf = id

{- |
Module      : Web.Api.Http.Shell
Description : Functions for using GHCi as an interactive HTTP shell.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX
-}

module Web.Api.Http.Shell (
    initShell
  , httpShell
  ) where

import Data.IORef
  ( IORef, newIORef, readIORef, writeIORef )

import Web.Api.Http.Types
import Web.Api.Http.Monad

-- | Initialize a context for running an HTTP shell interaction.
initShell
  :: St st
  -> Env err log env
  -> IO (IORef (St st, Env err log env))
initShell st env =
  newIORef (st,env)

-- | Execute an `HttpSession` in a shell interaction.
httpShell
  :: (Show err)
  => IORef (St st, Env err log env)
  -> HttpSession IO err st log env a
  -> IO a
httpShell ref session = do
  (st1,env) <- readIORef ref
  (result, (st2,_)) <- execSession session (st1, env)
  writeIORef ref (st2,env)
  case result of
    Left err -> error $ show err
    Right ok -> return ok

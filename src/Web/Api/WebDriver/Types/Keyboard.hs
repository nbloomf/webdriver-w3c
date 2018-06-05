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
  = UnidentifiedKey
  | CancelKey
  | HelpKey
  | BackspaceKey
  | TabKey
  | ClearKey
  | ReturnKey
  | EnterKey
  | ShiftKey
  | ControlKey
  | AltKey
  | PauseKey
  | EscapeKey
  | PageUpKey
  | PageDownKey
  | EndKey
  | HomeKey
  | ArrowLeftKey
  | ArrowUpKey
  | ArrowRightKey
  | ArrowDownKey
  | InsertKey
  | DeleteKey
  | F1Key
  | F2Key
  | F3Key
  | F4Key
  | F5Key
  | F6Key
  | F7Key
  | F8Key
  | F9Key
  | F10Key
  | F11Key
  | F12Key
  | MetaKey
  | ZenkakuHankakuKey

-- | See <https://w3c.github.io/webdriver/webdriver-spec.html#keyboard-actions>.
keyToChar :: Key -> Char
keyToChar key = case key of
  UnidentifiedKey   -> '\xe000'
  CancelKey         -> '\xe001'
  HelpKey           -> '\xe002'
  BackspaceKey      -> '\xe003'
  TabKey            -> '\xe004'
  ClearKey          -> '\xe005'
  ReturnKey         -> '\xe006'
  EnterKey          -> '\xe007'
  ShiftKey          -> '\xe008'
  ControlKey        -> '\xe009'
  AltKey            -> '\xe00a'
  PauseKey          -> '\xe00b'
  EscapeKey         -> '\xe00c'
  PageUpKey         -> '\xe00e'
  PageDownKey       -> '\xe00f'
  EndKey            -> '\xe010'
  HomeKey           -> '\xe011'
  ArrowLeftKey      -> '\xe012'
  ArrowUpKey        -> '\xe013'
  ArrowRightKey     -> '\xe014'
  ArrowDownKey      -> '\xe015'
  InsertKey         -> '\xe016'
  DeleteKey         -> '\xe017'
  F1Key             -> '\xe031'
  F2Key             -> '\xe032'
  F3Key             -> '\xe033'
  F4Key             -> '\xe034'
  F5Key             -> '\xe035'
  F6Key             -> '\xe036'
  F7Key             -> '\xe037'
  F8Key             -> '\xe038'
  F9Key             -> '\xe039'
  F10Key            -> '\xe03a'
  F11Key            -> '\xe03b'
  F12Key            -> '\xe03c'
  MetaKey           -> '\xe03d'
  ZenkakuHankakuKey -> '\xe040'

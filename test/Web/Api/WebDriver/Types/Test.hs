{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Web.Api.WebDriver.Types.Test (
    tests
  ) where

import Data.Proxy
import qualified Data.Aeson as A
  ( ToJSON(..), FromJSON, fromJSON, Result(..), object, Value(..) )

import Test.Tasty (TestTree(), testGroup)
import Test.Tasty.QuickCheck as QC (testProperty)
import Test.Tasty.HUnit as HU

import Web.Api.WebDriver.Types



tests :: TestTree
tests = testGroup "Web.Api.WebDriver.Types"
  [ test_fromJson_toJson_id
  , test_empty_objects
  , test_fromJson_parse_error
  ]



-- | Tests the round trip from value to encoded JSON and back.
prop_fromJson_toJson_id
  :: (Eq a, Show a, A.ToJSON a, A.FromJSON a)
  => a -> Bool
prop_fromJson_toJson_id x =
  case A.fromJSON $ A.toJSON x of
    A.Error _ -> False
    A.Success y -> x == y

test_fromJson_toJson_id :: TestTree
test_fromJson_toJson_id = testGroup "fromJSON . toJSON == id"
  [ QC.testProperty "(Capabilities) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: Capabilities -> Bool)

  , QC.testProperty "(BrowserName) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: BrowserName -> Bool)

  , QC.testProperty "(PlatformName) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: PlatformName -> Bool)

  , QC.testProperty "(FirefoxOptions) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: FirefoxOptions -> Bool)

  , QC.testProperty "(ChromeOptions) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: ChromeOptions -> Bool)

  , QC.testProperty "(ProxyConfig) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: ProxyConfig -> Bool)

  , QC.testProperty "(ProxyType) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: ProxyType -> Bool)

  , QC.testProperty "(HostAndOptionalPort) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: HostAndOptionalPort -> Bool)

  , QC.testProperty "(TimeoutConfig) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: TimeoutConfig -> Bool)

  , QC.testProperty "(InputSource) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: InputSource -> Bool)

  , QC.testProperty "(PointerSubtype) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: PointerSubtype -> Bool)

  , QC.testProperty "(InputSourceParameter) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: InputSourceParameter -> Bool)

  , QC.testProperty "(Action) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: Action -> Bool)

  , QC.testProperty "(ActionType) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: ActionType -> Bool)

  , QC.testProperty "(ActionItem) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: ActionItem -> Bool)

  , QC.testProperty "(LocationStrategy) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: LocationStrategy -> Bool)

  , QC.testProperty "(Rect) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: Rect -> Bool)

  , QC.testProperty "(PromptHandler) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: PromptHandler -> Bool)

  , QC.testProperty "(Cookie) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: Cookie -> Bool)

  , QC.testProperty "(ResponseErrorCode) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: ResponseErrorCode -> Bool)

  , QC.testProperty "(ContextId) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: ContextId -> Bool)

  , QC.testProperty "(ContextType) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: ContextType -> Bool)

  , QC.testProperty "(PrintOptions) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: PrintOptions -> Bool)

  , QC.testProperty "(Orientation) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: Orientation -> Bool)

  , QC.testProperty "(Scale) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: Scale -> Bool)

  , QC.testProperty "(Page) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: Page -> Bool)

  , QC.testProperty "(Margin) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: Margin -> Bool)

  , QC.testProperty "(PageRange) fromJSON . toJSON == id" $
      (prop_fromJson_toJson_id :: PageRange -> Bool)
  ]



-- | Empty objects are equivalent to a parsed "object []".
prop_is_empty_object
  :: (Eq a, Show a, A.ToJSON a)
  => a -> IO ()
prop_is_empty_object x = do
  let obj = A.toJSON x
  if obj == A.object []
    then return ()
    else assertFailure $ "Expected empty object; got " ++ show obj

test_empty_objects :: TestTree
test_empty_objects = testGroup "empty objects"
  [ HU.testCase "emptyCapabilities is an empty object" $
      prop_is_empty_object emptyCapabilities

  , HU.testCase "emptyProxyConfig is an empty object" $
      prop_is_empty_object emptyProxyConfig

  , HU.testCase "emptyTimeoutConfig is an empty object" $
      prop_is_empty_object emptyTimeoutConfig

  , HU.testCase "emptyActionItem is an empty object" $
      prop_is_empty_object emptyActionItem

  , HU.testCase "emptyCookie is an empty object" $
      prop_is_empty_object emptyCookie
  ]



-- | JSON parse errors.
prop_fromJson_parse_error
  :: (Eq a, Show a, A.FromJSON a)
  => Proxy a -> A.Value -> IO ()
prop_fromJson_parse_error x str =
  case A.fromJSON str of
    A.Error !_ -> return ()
    A.Success !y -> do
      let _ = asProxyTypeOf y x
      assertFailure $ "Expected parse failure!"

test_fromJson_parse_error :: TestTree
test_fromJson_parse_error = testGroup "JSON parse error expected"
  [ HU.testCase "BrowserName (unrecognized value)" $
      prop_fromJson_parse_error (Proxy :: Proxy BrowserName) $
      A.String "mosaic"

  , HU.testCase "BrowserName (wrong case)" $
      prop_fromJson_parse_error (Proxy :: Proxy BrowserName) $
      A.String "FIREFOX"

  , HU.testCase "BrowserName (wrong type - object)" $
      prop_fromJson_parse_error (Proxy :: Proxy BrowserName) $
      A.object [("browser","firefox")]

  , HU.testCase "BrowserName (wrong type - bool)" $
      prop_fromJson_parse_error (Proxy :: Proxy BrowserName) $
      A.Bool True


  , HU.testCase "PlatformName (unrecognized value)" $
      prop_fromJson_parse_error (Proxy :: Proxy PlatformName) $
      A.String "os/2"

  , HU.testCase "PlatformName (wrong case)" $
      prop_fromJson_parse_error (Proxy :: Proxy PlatformName) $
      A.String "MAC"

  , HU.testCase "PlatformName (wrong type - object)" $
      prop_fromJson_parse_error (Proxy :: Proxy PlatformName) $
      A.object [("platform","windows")]

  , HU.testCase "PlatformName (wrong type - bool)" $
      prop_fromJson_parse_error (Proxy :: Proxy PlatformName) $
      A.Bool True


  , HU.testCase "HostAndOptionalPort (malformed value - ':')" $
      prop_fromJson_parse_error (Proxy :: Proxy HostAndOptionalPort) $
      A.String ":"

  , HU.testCase "HostAndOptionalPort (malformed value - '@:123')" $
      prop_fromJson_parse_error (Proxy :: Proxy HostAndOptionalPort) $
      A.String "@:123"

  , HU.testCase "HostAndOptionalPort (malformed value - 'host:')" $
      prop_fromJson_parse_error (Proxy :: Proxy HostAndOptionalPort) $
      A.String "host:"

  , HU.testCase "HostAndOptionalPort (malformed value - 'host:foo')" $
      prop_fromJson_parse_error (Proxy :: Proxy HostAndOptionalPort) $
      A.String "host:foo"

  , HU.testCase "HostAndOptionalPort (wrong type - object)" $
      prop_fromJson_parse_error (Proxy :: Proxy HostAndOptionalPort) $
      A.object [("platform","windows")]

  , HU.testCase "HostAndOptionalPort (wrong type - bool)" $
      prop_fromJson_parse_error (Proxy :: Proxy HostAndOptionalPort) $
      A.Bool True


  , HU.testCase "ProxyType (unrecognized value)" $
      prop_fromJson_parse_error (Proxy :: Proxy ProxyType) $
      A.String "lan"

  , HU.testCase "ProxyType (wrong case)" $
      prop_fromJson_parse_error (Proxy :: Proxy ProxyType) $
      A.String "PAC"

  , HU.testCase "ProxyType (wrong type - object)" $
      prop_fromJson_parse_error (Proxy :: Proxy ProxyType) $
      A.object [("proxy","what")]

  , HU.testCase "ProxyType (wrong type - bool)" $
      prop_fromJson_parse_error (Proxy :: Proxy ProxyType) $
      A.Bool True


  , HU.testCase "LocationStrategy (unrecognized value)" $
      prop_fromJson_parse_error (Proxy :: Proxy LocationStrategy) $
      A.String "tag"

  , HU.testCase "LocationStrategy (wrong case)" $
      prop_fromJson_parse_error (Proxy :: Proxy LocationStrategy) $
      A.String "CSS Selector"

  , HU.testCase "LocationStrategy (wrong type - object)" $
      prop_fromJson_parse_error (Proxy :: Proxy LocationStrategy) $
      A.object [("css","selector")]

  , HU.testCase "LocationStrategy (wrong type - bool)" $
      prop_fromJson_parse_error (Proxy :: Proxy LocationStrategy) $
      A.Bool True


  , HU.testCase "InputSource (unrecognized value)" $
      prop_fromJson_parse_error (Proxy :: Proxy InputSource) $
      A.String "keyboard"

  , HU.testCase "InputSource (wrong case)" $
      prop_fromJson_parse_error (Proxy :: Proxy InputSource) $
      A.String "NULL"

  , HU.testCase "InputSource (wrong type - object)" $
      prop_fromJson_parse_error (Proxy :: Proxy InputSource) $
      A.object [("key","board")]

  , HU.testCase "InputSource (wrong type - bool)" $
      prop_fromJson_parse_error (Proxy :: Proxy InputSource) $
      A.Bool True


  , HU.testCase "PointerSubtype (unrecognized value)" $
      prop_fromJson_parse_error (Proxy :: Proxy PointerSubtype) $
      A.String "stylus"

  , HU.testCase "PointerSubtype (wrong case)" $
      prop_fromJson_parse_error (Proxy :: Proxy PointerSubtype) $
      A.String "Mouse"

  , HU.testCase "PointerSubtype (wrong type - object)" $
      prop_fromJson_parse_error (Proxy :: Proxy PointerSubtype) $
      A.object [("track","pad")]

  , HU.testCase "PointerSubtype (wrong type - bool)" $
      prop_fromJson_parse_error (Proxy :: Proxy PointerSubtype) $
      A.Bool True


  , HU.testCase "ActionType (unrecognized value)" $
      prop_fromJson_parse_error (Proxy :: Proxy ActionType) $
      A.String "keypress"

  , HU.testCase "ActionType (wrong case)" $
      prop_fromJson_parse_error (Proxy :: Proxy ActionType) $
      A.String "Pause"

  , HU.testCase "ActionType (wrong type - object)" $
      prop_fromJson_parse_error (Proxy :: Proxy ActionType) $
      A.object [("click","button")]

  , HU.testCase "ActionType (wrong type - bool)" $
      prop_fromJson_parse_error (Proxy :: Proxy ActionType) $
      A.Bool True


  , HU.testCase "PromptHandler (unrecognized value)" $
      prop_fromJson_parse_error (Proxy :: Proxy PromptHandler) $
      A.String "check"

  , HU.testCase "PromptHandler (wrong case)" $
      prop_fromJson_parse_error (Proxy :: Proxy PromptHandler) $
      A.String "Dismiss"

  , HU.testCase "PromptHandler (wrong type - object)" $
      prop_fromJson_parse_error (Proxy :: Proxy PromptHandler) $
      A.object [("accept","alert")]

  , HU.testCase "PromptHandler (wrong type - bool)" $
      prop_fromJson_parse_error (Proxy :: Proxy PromptHandler) $
      A.Bool True
  ]

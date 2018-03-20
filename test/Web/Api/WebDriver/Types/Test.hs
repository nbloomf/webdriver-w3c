module Web.Api.WebDriver.Types.Test (
    tests
  ) where

import qualified Data.Aeson as A
  ( ToJSON(..), FromJSON, fromJSON, Result(..), object )
import Test.QuickCheck
  ( quickCheck, Arbitrary(..), label, Property )

import Test.Tasty (TestTree(), testGroup)
import Test.Tasty.QuickCheck as QC (testProperty)
import Test.Tasty.HUnit as HU

import Web.Api.WebDriver.Types



tests :: TestTree
tests = testGroup "Web.Api.WebDriver.Types"
  [ test_fromJson_toJson_id
  , test_empty_objects
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

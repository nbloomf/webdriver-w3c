module Web.Api.Http.Effects.Test (
    tests
  ) where

import Test.Tasty (TestTree(), testGroup)
import Test.Tasty.QuickCheck as QC (testProperty)
import Test.Tasty.HUnit as HU

import Web.Api.Http.Effects
import Web.Api.Http.Effects.Test.Mock
import Web.Api.Http.Effects.Test.Server



tests :: TestTree
tests = testGroup "Web.Api.Http.Effects"
  [
  ]

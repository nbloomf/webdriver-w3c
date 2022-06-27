{-# LANGUAGE OverloadedStrings #-}
module Test.Tasty.WebDriver.Config.Test (
    tests
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Test.Tasty as TT
import qualified Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS

import Test.Tasty.WebDriver.Config

tests :: TT.TestTree
tests = TT.testGroup "Test.Tasty.WebDriver.Config"
  [ check_parseRemoteEnd
  , check_parseRemoteEndOption
  , check_parseRemoteEndConfig
  ]

checkParser :: (Eq a, Show a) => (Text -> a) -> (String, Text, a) -> TT.TestTree
checkParser f (title, str, x) =
  let y = f str in
  HU.testCase title $
    if y == x
      then return ()
      else do
        HU.assertFailure $ "Error parsing '" ++ Text.unpack str ++ "': expected "
          ++ show x ++ " but got " ++ show y

check_parseRemoteEnd :: TT.TestTree
check_parseRemoteEnd = TT.testGroup "parseRemoteEnd" $
  map (checkParser parseRemoteEnd) _parseRemoteEnd_cases

_parseRemoteEnd_cases :: [(String, Text, Either Text RemoteEnd)]
_parseRemoteEnd_cases =
  [ ( "'localhost'"
    , "localhost"
    , Left "Could not parse remote end URI 'localhost'."
    )

  , ( "'localhost:4444'"
    , "localhost:4444"
    , Left "Error parsing authority for URI 'localhost:4444'."
    )

  , ( "'http://localhost:4444'"
    , "http://localhost:4444"
    , Right $ RemoteEnd "localhost" 4444 ""
    )

  , ( "'https://localhost:4444'"
    , "https://localhost:4444"
    , Right $ RemoteEnd "localhost" 4444 ""
    )

  , ( "'http://localhost'"
    , "http://localhost"
    , Right $ RemoteEnd "localhost" 4444 ""
    )

  , ( "'https://localhost'"
    , "https://localhost"
    , Right $ RemoteEnd "localhost" 4444 ""
    )
  ]

check_parseRemoteEndOption :: TT.TestTree
check_parseRemoteEndOption = TT.testGroup "parseRemoteEndOption" $
  map (checkParser parseRemoteEndOption) _parseRemoteEndOption_cases

_parseRemoteEndOption_cases :: [(String, Text, Either Text RemoteEndPool)]
_parseRemoteEndOption_cases =
  [ ( "geckodriver+1"
    , "geckodriver https://localhost:4444"
    , Right $ RemoteEndPool $
        MS.fromList [(Geckodriver, [RemoteEnd "localhost" 4444 ""])]
    )

  , ( "geckodriver+1 (repeated)"
    , "geckodriver https://localhost:4444 https://localhost:4444"
    , Right $ RemoteEndPool $
        MS.fromList [(Geckodriver, [RemoteEnd "localhost" 4444 ""])]
    )

  , ( "geckodriver+2"
    , "geckodriver https://localhost:4444 https://localhost:4445"
    , Right $ RemoteEndPool $
        MS.fromList [(Geckodriver, [RemoteEnd "localhost" 4444 "", RemoteEnd "localhost" 4445 ""])]
    )

  , ( "chromedriver+1"
    , "chromedriver https://localhost:4444"
    , Right $ RemoteEndPool $
        MS.fromList [(Chromedriver, [RemoteEnd "localhost" 4444 ""])]
    )

  , ( "chromedriver+1 (repeated)"
    , "chromedriver https://localhost:4444 https://localhost:4444"
    , Right $ RemoteEndPool $
        MS.fromList [(Chromedriver, [RemoteEnd "localhost" 4444 ""])]
    )

  , ( "chromedriver+2"
    , "chromedriver https://localhost:4444 https://localhost:4445"
    , Right $ RemoteEndPool $
        MS.fromList [(Chromedriver, [RemoteEnd "localhost" 4444 "", RemoteEnd "localhost" 4445 ""])]
    )

  , ( "geckodriver+1, chromedriver+1"
    , "geckodriver https://localhost:4444 chromedriver https://localhost:9515"
    , Right $ RemoteEndPool $
        MS.fromList
          [ (Geckodriver, [RemoteEnd "localhost" 4444 ""])
          , (Chromedriver, [RemoteEnd "localhost" 9515 ""])
          ]
    )
  ]

check_parseRemoteEndConfig :: TT.TestTree
check_parseRemoteEndConfig = TT.testGroup "parseRemoteEndConfig" $
  map (checkParser parseRemoteEndConfig) _parseRemoteEndConfig_cases

_parseRemoteEndConfig_cases :: [(String, Text, Either Text RemoteEndPool)]
_parseRemoteEndConfig_cases =
  [ ( "geckodriver+1"
    , "geckodriver\n- https://localhost:4444\n"
    , Right $ RemoteEndPool $
        MS.fromList [(Geckodriver, [RemoteEnd "localhost" 4444 ""])]
    )

  , ( "geckodriver+1 (repeated)"
    , "geckodriver\n- https://localhost:4444\n- https://localhost:4444\n"
    , Right $ RemoteEndPool $
        MS.fromList [(Geckodriver, [RemoteEnd "localhost" 4444 ""])]
    )

  , ( "geckodriver+2"
    , "geckodriver\n- https://localhost:4444\n- https://localhost:4445\n"
    , Right $ RemoteEndPool $
        MS.fromList [(Geckodriver, [RemoteEnd "localhost" 4444 "", RemoteEnd "localhost" 4445 ""])]
    )

  , ( "chromedriver+1"
    , "chromedriver\n- https://localhost:4444\n"
    , Right $ RemoteEndPool $
        MS.fromList [(Chromedriver, [RemoteEnd "localhost" 4444 ""])]
    )

  , ( "chromedriver+1 (repeated)"
    , "chromedriver\n- https://localhost:4444\n- https://localhost:4444\n"
    , Right $ RemoteEndPool $
        MS.fromList [(Chromedriver, [RemoteEnd "localhost" 4444 ""])]
    )

  , ( "chromedriver+2"
    , "chromedriver\n- https://localhost:4444\n- https://localhost:4445\n"
    , Right $ RemoteEndPool $
        MS.fromList [(Chromedriver, [RemoteEnd "localhost" 4444 "", RemoteEnd "localhost" 4445 ""])]
    )

  , ( "geckodriver+1, chromedriver+1"
    , "geckodriver\n- https://localhost:4444\nchromedriver\n- https://localhost:9515\n"
    , Right $ RemoteEndPool $
        MS.fromList
          [ (Geckodriver, [RemoteEnd "localhost" 4444 ""])
          , (Chromedriver, [RemoteEnd "localhost" 9515 ""])
          ]
    )
  ]

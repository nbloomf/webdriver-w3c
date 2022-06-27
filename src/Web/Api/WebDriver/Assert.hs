{- |
Module      : Web.Api.WebDriver.Assert
Description : Mini language for making falsifiable assertions.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

In this module we define assertions as first class objects and some helper functions for creating and manipulating them.
-}

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Web.Api.WebDriver.Assert (
  -- * Assertions
    Assertion()
  , success
  , failure
  , AssertionStatement(..)
  , AssertionComment(..)
  , AssertionResult()
  , isSuccess
  , printAssertion

  -- * The `Assert` Class
  , Assert(..)

  -- * Assertion Summaries
  , AssertionSummary(..)
  , summarize
  , summarizeAll
  , printSummary
  , numAssertions

  -- * Basic Assertions
  , assertSuccessIf
  , assertSuccess
  , assertFailure
  , assertTrue
  , assertFalse
  , assertEqual
  , assertNotEqual
  , assertIsSubstring
  , assertIsNotSubstring
  , assertIsNamedSubstring
  , assertIsNotNamedSubstring
  ) where

import Data.List
  ( isInfixOf )
import Data.String
  ( IsString, fromString )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Test.QuickCheck
  ( Arbitrary(..) )



-- | An `Assertion` consists of the following:
--
-- * A human-readable statement being asserted, which may be either true or false.
-- * A result (either success or failure).
-- * A comment, representing /why/ the assertion was made, to assist in debugging.
--
-- To construct assertions outside this module, use `success` and `failure`.

data Assertion = Assertion
  { assertionStatement :: AssertionStatement
  , assertionComment :: AssertionComment
  , assertionResult :: AssertionResult
  } deriving (Eq, Show)



-- | Human-readable statement which may be true or false.
newtype AssertionStatement = AssertionStatement
  { theAssertionStatement :: Text
  } deriving Eq

instance Show AssertionStatement where
  show = T.unpack . theAssertionStatement

instance IsString AssertionStatement where
  fromString = AssertionStatement . T.pack

instance Arbitrary AssertionStatement where
  arbitrary = AssertionStatement <$> (fmap T.pack arbitrary)



-- | Human-readable explanation for why an assertion is made.
newtype AssertionComment = AssertionComment
  { theAssertionComment :: Text
  } deriving Eq

instance Show AssertionComment where
  show = T.unpack . theAssertionComment

instance IsString AssertionComment where
  fromString = AssertionComment . T.pack

instance Arbitrary AssertionComment where
  arbitrary = AssertionComment <$> (fmap T.pack arbitrary)



-- | Type representing the result (success or failure) of an evaluated assertion.
data AssertionResult
  = AssertSuccess | AssertFailure
  deriving (Eq, Show)

instance Arbitrary AssertionResult where
  arbitrary = do
    p <- arbitrary
    return $ if p then AssertSuccess else AssertFailure

-- | Detects successful assertions.
isSuccess :: Assertion -> Bool
isSuccess a = AssertSuccess == assertionResult a



-- | Basic string representation of an assertion.
printAssertion :: Assertion -> Text
printAssertion Assertion{..} =
  case assertionResult of
    AssertSuccess -> 
      T.unwords
        [ "\x1b[1;32mValid Assertion\x1b[0;39;49m"
        , "\nassertion: " <> theAssertionStatement assertionStatement
        , "\ncomment: " <> theAssertionComment assertionComment
        ]
    AssertFailure ->
      T.unwords
        [ "\x1b[1;31mInvalid Assertion\x1b[0;39;49m"
        , "\nassertion: " <> theAssertionStatement assertionStatement
        , "\ncomment: " <> theAssertionComment assertionComment
        ]



-- | Construct a successful assertion.
success
  :: AssertionStatement -- ^ Statement being asserted (the /what/)
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> Assertion
success statement comment = Assertion
  { assertionStatement = statement
  , assertionComment = comment
  , assertionResult = AssertSuccess
  }

-- | Construct a failed assertion.
failure
  :: AssertionStatement -- ^ Statement being asserted (the /what/)
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> Assertion
failure statement comment = Assertion
  { assertionStatement = statement
  , assertionComment = comment
  , assertionResult = AssertFailure
  }



-- | Assertions are made and evaluated inside some context, represented by the `Assert` class.
class Assert m where
  -- | Make an assertion. Typically @m@ is a monad, and the `Assert` instance handles the assertion in @m@ by e.g. logging it, changing state, etc.
  assert :: Assertion -> m ()



-- | Generic boolean assertion; asserts success if @Bool@ is true and failure otherwise.
assertSuccessIf
  :: (Monad m, Assert m)
  => Bool
  -> AssertionStatement -- ^ Statement being asserted (the /what/)
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertSuccessIf p statement comment =
  assert $ (if p then success else failure) statement comment

-- | Assertion that always succeeds.
assertSuccess
  :: (Monad m, Assert m)
  => AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertSuccess = assertSuccessIf True (AssertionStatement "Success!")

-- | Assertion that always fails.
assertFailure
  :: (Monad m, Assert m)
  => AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertFailure = assertSuccessIf False (AssertionStatement "Failure :(")

-- | Succeeds if @Bool@ is `True`.
assertTrue
  :: (Monad m, Assert m)
  => Bool
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertTrue p = assertSuccessIf p
  (AssertionStatement $ T.pack (show p) <> " is True")

-- | Succeeds if @Bool@ is `False`.
assertFalse
  :: (Monad m, Assert m)
  => Bool
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertFalse p = assertSuccessIf (not p)
  (AssertionStatement $ T.pack (show p) <> " is False")

-- | Succeeds if the given @t@s are equal according to their `Eq` instance.
assertEqual
  :: (Monad m, Assert m, Eq t, Show t)
  => t
  -> t
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertEqual x y = assertSuccessIf (x == y)
  (AssertionStatement $
    T.pack (show x) <> " is equal to " <> T.pack (show y))

-- | Succeeds if the given @t@s are not equal according to their `Eq` instance.
assertNotEqual
  :: (Monad m, Assert m, Eq t, Show t)
  => t
  -> t
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertNotEqual x y = assertSuccessIf (x /= y)
  (AssertionStatement $ T.pack (show x) <> " is not equal to " <> T.pack (show y))

-- | Succeeds if the first list is an infix of the second, according to their `Eq` instance.
assertIsSubstring
  :: (Monad m, Assert m)
  => Text
  -> Text
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertIsSubstring x y = assertSuccessIf (T.isInfixOf x y)
  (AssertionStatement $ T.pack (show x) <> " is a substring of " <> T.pack (show y))

-- | Succeeds if the first list is not an infix of the second, according to their `Eq` instance.
assertIsNotSubstring
  :: (Monad m, Assert m)
  => Text
  -> Text
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertIsNotSubstring x y = assertSuccessIf (not $ T.isInfixOf x y)
  (AssertionStatement $ T.pack (show x) <> " is not a substring of " <> T.pack (show y))

-- | Succeeds if the first list is an infix of the second, named list, according to their `Eq` instance. This is similar to `assertIsSubstring`, except that the "name" of the second list argument is used in reporting failures. Handy if the second list is very large -- say the source of a webpage.
assertIsNamedSubstring
  :: (Monad m, Assert m)
  => Text
  -> (Text,Text)
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertIsNamedSubstring x (y,name) = assertSuccessIf (T.isInfixOf x y)
  (AssertionStatement $ T.pack (show x) <> " is a substring of " <> name)

-- | Succeeds if the first list is not an infix of the second, named list, according to their `Eq` instance. This is similar to `assertIsNotSubstring`, except that the "name" of the second list argument is used in reporting failures. Handy if the second list is very large -- say the source of a webpage.
assertIsNotNamedSubstring
  :: (Monad m, Assert m)
  => Text
  -> (Text,Text)
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertIsNotNamedSubstring x (y,name) = assertSuccessIf (not $ T.isInfixOf x y)
  (AssertionStatement $ T.pack (show x) <> " is not a substring of " <> name)



-- | `Assertion`s are the most granular kind of "test" this library deals with. Typically we'll be interested in sets of many assertions. A single test case will include one or more assertions, which for reporting purposes we'd like to summarize. The summary for a list of assertions will include the number of successes, the number of failures, and the actual failures. Modeled this way assertion summaries form a monoid, which is handy.

data AssertionSummary = AssertionSummary
  { numSuccesses :: Integer
  , numFailures :: Integer
  , failures :: [Assertion]
  , successes :: [Assertion]
  } deriving (Eq, Show)

instance Semigroup AssertionSummary where
  x <> y = AssertionSummary
    { numSuccesses = numSuccesses x + numSuccesses y
    , numFailures = numFailures x + numFailures y
    , failures = failures x ++ failures y
    , successes = successes x ++ successes y
    }

instance Monoid AssertionSummary where
  mempty = AssertionSummary 0 0 [] []

  mappend = (<>)

-- | Summarize a single assertion.
summary :: Assertion -> AssertionSummary
summary x = AssertionSummary
  { numSuccesses = if isSuccess x then 1 else 0
  , numFailures = if isSuccess x then 0 else 1
  , failures = if isSuccess x then [] else [x]
  , successes = if isSuccess x then [x] else []
  }

-- | Summarize a list of `Assertion`s.
summarize :: [Assertion] -> AssertionSummary
summarize = mconcat . map summary

-- | Summarize a list of `AssertionSummary`s.
summarizeAll :: [AssertionSummary] -> AssertionSummary
summarizeAll = mconcat

-- | Very basic string representation of an `AssertionSummary`.
printSummary :: AssertionSummary -> IO ()
printSummary AssertionSummary{..} = do
  mapM_ (T.putStrLn . printAssertion) failures
  putStrLn $ "Assertions: " ++ show (numSuccesses + numFailures)
  putStrLn $ "Failures: " ++ show numFailures

-- | Total number of assertions made.
numAssertions :: AssertionSummary -> Integer
numAssertions x = numSuccesses x + numFailures x

{- |
Module      : Web.Api.Http.Assert
Description : Mini language for making falsifiable assertions.
Copyright   : 2018, Automattic, Inc.
License     : GPL-3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

In this module we define assertions as first class objects and some helper functions for creating and manipulating them.
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Api.Http.Assert (
  -- * Assertions
    Assertion()
  , success
  , failure
  , AssertionStatement()
  , AssertionComment()
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
  ( unwords, isInfixOf )
import Data.String
  ( IsString, fromString )



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
data AssertionStatement = AssertionStatement
  { theAssertionStatement :: String
  } deriving Eq

instance Show AssertionStatement where
  show = theAssertionStatement

instance IsString AssertionStatement where
  fromString = AssertionStatement


-- | Human-readable explanation for why an assertion is made.
data AssertionComment = AssertionComment
  { theAssertionComment :: String
  } deriving Eq

instance Show AssertionComment where
  show = theAssertionComment

instance IsString AssertionComment where
  fromString = AssertionComment


-- | Type representing the result (success or failure) of an evaluated assertion.
data AssertionResult
  = AssertSuccess | AssertFailure
  deriving (Eq, Show)

-- | Detects successful assertions.
isSuccess :: Assertion -> Bool
isSuccess a = AssertSuccess == assertionResult a


-- | Basic string representation of an assertion.
printAssertion :: Assertion -> String
printAssertion Assertion{..} =
  case assertionResult of
    AssertSuccess -> 
      unwords
        [ "\x1b[1;32mValid Assertion\x1b[0;39;49m"
        , "\nassertion: " ++ show assertionStatement
        , "\ncomment: " ++ show assertionComment
        ]
    AssertFailure ->
      unwords
        [ "\x1b[1;31mInvalid Assertion\x1b[0;39;49m"
        , "\nassertion: " ++ show assertionStatement
        , "\ncomment: " ++ show assertionComment
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
assertSuccessIf p statement comment = do
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
assertTrue p = assertSuccessIf (p == True)
  (AssertionStatement $ show p ++ " is True")

-- | Succeeds if @Bool@ is `False`.
assertFalse
  :: (Monad m, Assert m)
  => Bool
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertFalse p = assertSuccessIf (p == False)
  (AssertionStatement $ show p ++ " is False")

-- | Succeeds if the given @t@s are equal according to their `Eq` instance.
assertEqual
  :: (Monad m, Assert m, Eq t, Show t)
  => t
  -> t
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertEqual x y = assertSuccessIf (x == y)
  (AssertionStatement $ show x ++ " is equal to " ++ show y)

-- | Succeeds if the given @t@s are not equal according to their `Eq` instance.
assertNotEqual
  :: (Monad m, Assert m, Eq t, Show t)
  => t
  -> t
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertNotEqual x y = assertSuccessIf (x /= y)
  (AssertionStatement $ show x ++ " is not equal to " ++ show y)

-- | Succeeds if the first list is an infix of the second, according to their `Eq` instance.
assertIsSubstring
  :: (Monad m, Assert m, Eq a, Show a)
  => [a]
  -> [a]
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertIsSubstring x y = assertSuccessIf (isInfixOf x y)
  (AssertionStatement $ show x ++ " is a substring of " ++ show y)

-- | Succeeds if the first list is not an infix of the second, according to their `Eq` instance.
assertIsNotSubstring
  :: (Monad m, Assert m, Eq a, Show a)
  => [a]
  -> [a]
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertIsNotSubstring x y = assertSuccessIf (not $ isInfixOf x y)
  (AssertionStatement $ show x ++ " is not a substring of " ++ show y)

-- | Succeeds if the first list is an infix of the second, named list, according to their `Eq` instance. This is similar to `assertIsSubstring`, except that the "name" of the second list argument is used in reporting failures. Handy if the second list is very large -- say the source of a webpage.
assertIsNamedSubstring
  :: (Monad m, Assert m, Eq a, Show a)
  => [a]
  -> ([a],String)
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertIsNamedSubstring x (y,name) = assertSuccessIf (isInfixOf x y)
  (AssertionStatement $ show x ++ " is a substring of " ++ name)

-- | Succeeds if the first list is not an infix of the second, named list, according to their `Eq` instance. This is similar to `assertIsNotSubstring`, except that the "name" of the second list argument is used in reporting failures. Handy if the second list is very large -- say the source of a webpage.
assertIsNotNamedSubstring
  :: (Monad m, Assert m, Eq a, Show a)
  => [a]
  -> ([a],String)
  -> AssertionComment -- ^ An additional comment (the /why/)
  -> m ()
assertIsNotNamedSubstring x (y,name) = assertSuccessIf (not $ isInfixOf x y)
  (AssertionStatement $ show x ++ " is not a substring of " ++ name)



-- | `Assertion`s are the most granular kind of "test" this library deals with. Typically we'll be interested in sets of many assertions. A single test case will include one or more assertions, which for reporting purposes we'd like to summarize. The summary for a list of assertions will include the number of successes, the number of failures, and the actual failures. Modeled this way assertion summaries form a monoid, which is handy.

data AssertionSummary = AssertionSummary
  { numSuccesses :: Integer
  , numFailures :: Integer
  , failures :: [Assertion]
  } deriving (Eq, Show)

instance Monoid AssertionSummary where
  mempty = AssertionSummary 0 0 []

  mappend x y = AssertionSummary
    { numSuccesses = numSuccesses x + numSuccesses y
    , numFailures = numFailures x + numFailures y
    , failures = failures x ++ failures y
    }

-- | Summarize a single assertion.
summary :: Assertion -> AssertionSummary
summary x = AssertionSummary
  { numSuccesses = if isSuccess x then 1 else 0
  , numFailures = if isSuccess x then 0 else 1
  , failures = if isSuccess x then [] else [x]
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
  mapM_ (putStrLn . printAssertion) failures
  putStrLn $ "Assertions: " ++ show (numSuccesses + numFailures)
  putStrLn $ "Failures: " ++ show numFailures

-- | Total number of assertions made.
numAssertions :: AssertionSummary -> Integer
numAssertions x = numSuccesses x + numFailures x

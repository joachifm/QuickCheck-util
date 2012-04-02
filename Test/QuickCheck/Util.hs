-- |
-- Module     : Test.QuickCheck.Util
--
-- Copyright  : (C) 2010 Joachim Fasting
-- License    : BSD-style (see COPYING)
-- Maintainer : Joachim Fasting <joachim.fasting@gmail.com>
--
-- A collection of small, composable QuickCheck utilities.

module Test.QuickCheck.Util (
    runGenIO, propertyM,
    -- * Combinators
    -- $combinators
    pairOf, tripleOf, possibly,
    -- * Producing values
    -- $producers
    positive, negative
    ) where

-- Notes:
-- Prefer applicative over monad
-- Prefer functions over data
-- Prefer general over specific
-- Absolutely no orphan instances

import Control.Applicative
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary(..), Property)
import Test.QuickCheck.Gen (Gen(..))
import Test.QuickCheck.Monadic (monadicIO, run)
import System.Random (newStdGen, randomR)

-- | Check impure code.
--
-- This should probably have a name that is harder to type.
propertyM :: IO a -> Property
propertyM = monadicIO . run

-- | Generate random data.
--
-- Can be used for \"fuzzing\" or to inspect the values created by a 'Gen'.
--
-- Example: create a list of 1000 arbitrary positive numbers:
--
--    > runGenIO $ vectorOf 1000 positive
runGenIO :: Gen a -> IO a
runGenIO (MkGen m) = uncurry (flip m) . randomR (0, 500) <$> newStdGen

------------------------------------------------------------------------------
-- $combinators
-----------------------------------------------------------------------------

-- | Create a pair generator.
pairOf :: Applicative m => m a -> m (a, a)
pairOf m = (,) <$> m <*> m

-- | Create a triple generator.
tripleOf :: Applicative m => m a -> m (a, a, a)
tripleOf m = (,,) <$> m <*> m <*> m

-- | Turn a value generator into a generator that _might_ generate a value.
--
-- Example:
--
-- @possibly $ tripleOf negative@
possibly :: Gen a -> Gen (Maybe a)
possibly m = QC.oneof [ Just <$> m , pure Nothing ]

------------------------------------------------------------------------------
-- $producers
------------------------------------------------------------------------------

-- | Produce an arbitrary, positive number.
positive :: (Arbitrary a, Num a, Ord a) => Gen a
positive = QC.suchThat arbitrary (> 0)

-- | Produce an arbitrary, negative number.
negative :: (Arbitrary a, Ord a, Num a) => Gen a
negative = QC.suchThat arbitrary (< 0)

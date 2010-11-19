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
    pair, triple, possibly, mapping,
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
import Data.Char
import Control.Monad
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
runGenIO :: Gen a -> IO a
runGenIO (MkGen m) = uncurry (flip m) . randomR (0, 500) <$> newStdGen

------------------------------------------------------------------------------
-- $combinators
-----------------------------------------------------------------------------

-- | Create a pair generator.
pair :: Applicative m => m a -> m (a, a)
pair m = (,) <$> m <*> m

-- | Create a triple generator.
triple :: Applicative m => m a -> m (a, a, a)
triple m = (,,) <$> m <*> m <*> m

-- | Turn a value generator into a generator that _might_ generate a value.
--
-- Example:
--
-- @possibly $ triple negative@
possibly :: Gen a -> Gen (Maybe a)
possibly m = arbitrary >>= bool (Just <$> m) (return Nothing)
    where bool thenE elseE b = if b then thenE else elseE

-- | Create a mapping generator that produces an infinite list of mappings
-- from arbitrary keys to arbitrary values.
--
-- Example:
--
-- @take 5 \<$\> mapping negative (possibly positive)@
mapping :: (Arbitrary key, Arbitrary value) => Gen key -> Gen value -> Gen [(key, value)]
mapping km vm = (:) <$> ((,) <$> km <*> vm) <*> mapping km vm

------------------------------------------------------------------------------
-- $producers
------------------------------------------------------------------------------

-- | Produce an arbitrary, positive number.
positive :: (Arbitrary a, Num a) => Gen a
positive = abs <$> arbitrary

-- | Produce an arbitrary, negative number.
negative :: (Arbitrary a, Num a) => Gen a
negative = neg <$> arbitrary
    where neg x = if signum x /= -1 then negate x else x
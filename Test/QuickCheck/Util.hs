-- |
-- Module     : Test.QuickCheck.Util
--
-- Copyright  : (C) 2010 Joachim Fasting
-- License    : BSD-style (see COPYING)
-- Maintainer : Joachim Fasting <joachim.fasting@gmail.com>
--
-- A collection of small, composable QuickCheck utilities.

module Test.QuickCheck.Util (
    -- * I/O utilities
    module Test.QuickCheck.Util.IO,
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

import Test.QuickCheck.Util.IO

import Control.Applicative
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Gen (Gen(..))

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

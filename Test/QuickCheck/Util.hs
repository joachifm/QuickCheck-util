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
    module Test.QuickCheck.Util.Combinator,
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
import Test.QuickCheck.Util.Combinator

import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary, Gen)

------------------------------------------------------------------------------
-- $producers
------------------------------------------------------------------------------

-- | Produce an arbitrary, positive number.
positive :: (Arbitrary a, Num a, Ord a) => Gen a
positive = QC.suchThat QC.arbitrary (> 0)

-- | Produce an arbitrary, negative number.
negative :: (Arbitrary a, Ord a, Num a) => Gen a
negative = QC.suchThat QC.arbitrary (< 0)

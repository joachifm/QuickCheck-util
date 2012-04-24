-- |
-- Module     : Test.QuickCheck.Util.Gen
--
-- Copyright  : (C) 2010-2012 Joachim Fasting
-- License    : BSD-style (see COPYING)
-- Maintainer : Joachim Fasting <joachim.fasting@gmail.com>
--
-- Additional generators for QuickCheck.

module Test.QuickCheck.Util.Gen
    ( positive
    , negative
    ) where

import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary, Gen)

-- | Generate an arbitrary, positive number.
positive :: (Arbitrary a, Num a, Ord a) => Gen a
positive = QC.suchThat QC.arbitrary (> 0)

-- | Generate an arbitrary, negative number.
negative :: (Arbitrary a, Ord a, Num a) => Gen a
negative = QC.suchThat QC.arbitrary (< 0)

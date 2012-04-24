-- |
-- Module     : Test.QuickCheck.Util.IO
--
-- Copyright  : (C) 2010-2012 Joachim Fasting
-- License    : BSD-style (see COPYING)
-- Maintainer : Joachim Fasting <joachim.fasting@gmail.com>
--
-- I/O utilities for QuickCheck.

module Test.QuickCheck.Util.IO
    ( propertyM
    , runGenIO
    ) where

import Control.Applicative
import Test.QuickCheck (Property)
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
--
-- Note that this is different from 'sample'' in that it doesn't return
-- a sample of values but all the values that a _single run_ of the
-- generator would produce.
runGenIO :: Gen a -> IO a
runGenIO (MkGen m) = uncurry (flip m) . randomR (0, 500) <$> newStdGen

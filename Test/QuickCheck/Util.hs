-- |
-- Module     : Test.QuickCheck.Util
--
-- Copyright  : (C) 2010-2012 Joachim Fasting
-- License    : BSD-style (see COPYING)
-- Maintainer : Joachim Fasting <joachim.fasting@gmail.com>
--
-- A collection of small, composable QuickCheck utilities.

module Test.QuickCheck.Util (
    -- * I/O utilities
    module Test.QuickCheck.Util.IO,
    -- * Combinators
    module Test.QuickCheck.Util.Combinator,
    -- * Generators
    module Test.QuickCheck.Util.Gen
    ) where

-- Notes:
-- Prefer applicative over monad
-- Prefer functions over data
-- Prefer general over specific
-- Absolutely no orphan instances

import Test.QuickCheck.Util.IO
import Test.QuickCheck.Util.Combinator
import Test.QuickCheck.Util.Gen

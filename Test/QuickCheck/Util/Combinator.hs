-- |
-- Module     : Test.QuickCheck.Util.Combinator
--
-- Copyright  : (C) 2010-2012 Joachim Fasting
-- License    : BSD-style (see COPYING)
-- Maintainer : Joachim Fasting <joachim.fasting@gmail.com>
--
-- Additional combinators for QuickCheck.

module Test.QuickCheck.Util.Combinator
    ( pairOf
    , tripleOf
    , possibly
    ) where

import Control.Applicative
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

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

-- |
-- Module      : Data.Vector.Shuffle
-- Copyright   : (c) Alexey Kuleshevich 2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Vector.Shuffle
  ( uniformShuffle
  , uniformShuffleM
  ) where

import Control.Monad.Primitive
import Data.Vector.Generic as VG
import Data.Vector.Mutable.Shuffle
import System.Random.Stateful

uniformShuffle :: (VG.Vector v a, RandomGen g) => v a -> g -> (v a, g)
uniformShuffle v g = runStateGenST g (uniformShuffleM v)
{-# INLINE uniformShuffle #-}


uniformShuffleM :: (Vector v a, StatefulGen g m, PrimMonad m) => v a -> g -> m (v a)
uniformShuffleM v mg = do
  mv <- VG.thaw v
  uniformShuffleM_ mv mg
  VG.unsafeFreeze mv
{-# INLINE uniformShuffleM #-}

{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Vector.Mutable.Shuffle
-- Copyright   : (c) Alexey Kuleshevich 2022
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Vector.Mutable.Shuffle
  ( uniformShuffleM_
  ) where

import Control.Monad.Primitive
import Data.Vector.Generic.Mutable as VGM
import System.Random.Stateful


uniformShuffleM_ ::
     (StatefulGen g m, PrimMonad m, VGM.MVector v a)
  => v (PrimState m) a
  -> g
  -> m ()
uniformShuffleM_ vec gen = go (VGM.length vec - 1)
  where
    go !lastIx
      | lastIx < 1 = pure ()
      | otherwise = do
        curIx <- uniformRM (0, lastIx) gen
        VGM.unsafeSwap vec curIx lastIx
        go (lastIx - 1)
{-# INLINE uniformShuffleM_ #-}

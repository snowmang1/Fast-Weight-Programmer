module Slow (
  analyzeBoard
  ) where

import Board (Board (Red, White, WhiteKing, RedKing, Empty))
import Lib (mapMat)
import Data.Matrix

{-TODO:
  [ ] analzeWin function
  [ ] modulate weighted matrix
    - function for map that can modulate the existing weights of
      the current weight matrix (same type as analyzeWin)
-}

-- | single window analysis. take a window and make decisions based on the center piece.
-- form opinion in the form of a set of weights, function designed for 'mapMat'
analyzeWin :: Int -> Int -> Matrix Board -> b
analyzeWin = undefined

-- | analyzation step ran by the slow matrix on each itteration
analyzeBoard :: Matrix Board -> Maybe (Matrix Board)
analyzeBoard m = undefined

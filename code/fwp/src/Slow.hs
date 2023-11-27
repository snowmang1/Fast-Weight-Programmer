module Slow (
  analyzeBoard
  ) where

import Board (Board (Red, White, WhiteKing, RedKing, Empty))
import Data.Matrix

analyzeBoard :: Matrix Board -> Maybe (Matrix Board)
analyzeBoard m = Nothing

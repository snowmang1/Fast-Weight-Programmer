module Main (main) where

import qualified Lib
import qualified Board
import qualified Slow
import Data.Matrix

main :: IO ()
main = print "hello world"

iterGameFWP :: Matrix Board.Board -> Matrix Lib.Weights -> Matrix Board.Board
iterGameFWP = undefined

gameBoardDM  :: Matrix Board.Board
gameBoardDM = Lib.setBoard

gameBoardFWP :: Matrix Board.Board
gameBoardFWP = Lib.setBoard

playerDM  :: Matrix Lib.Weights
playerDM = Lib.mapMat gameBoardDM (\r c m-> if m!(r,c) == Board.RedKing || m!(r,c) == Board.WhiteKing then (1/6,1/6,1/6,1/6,1/6,1/6) else (1/6,0,1/6,1/6,0,0))

playerFWP :: Matrix Lib.Weights
playerFWP = Slow.analyzeBoard gameBoardFWP playerDM

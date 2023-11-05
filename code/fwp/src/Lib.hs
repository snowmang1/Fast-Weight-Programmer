module Lib
    ( someFunc,
    setBoard,
    setWeights,
    mapBoard,
    Player (White, Red),
    Board (Piece, Empty)
    ) where

-- TODO make mapBoard more general as a map over matrix board that returns a 'matrix a'

import Data.Matrix

data Player = White | Red deriving (Eq,Show)
data Board  = Piece Player | Empty deriving (Eq,Show)

type Weights = (Float, Float, Float, Float) -- cap left, cap right, forward, backward

someFunc :: IO ()
someFunc = putStrLn "not ready for testing"

oscilateRow :: Int -> Int -> Bool -> [Board] -> [Board]
oscilateRow _ 0 _ ac = reverse ac
oscilateRow t c b ac = if mod c 2==t then oscilateRow t (c-1) b (if b then Piece White:ac else Piece Red:ac) else oscilateRow t (c-1) b (Empty:ac)
-- | 'setBoard' Initializes the Board
setBoard :: Matrix Board
setBoard =
  fromLists (red ++ mid ++ white) where
  red = [oddtile,eventile,oddtile] where
    eventile = oscilateRow 0 8 False []
    oddtile = oscilateRow 1 8 False []
  mid = replicate 2 (replicate 8 Empty)
  white = [eventile,oddtile,eventile] where
    eventile = oscilateRow 0 8 True []
    oddtile = oscilateRow 1 8 True []

mapBoardAux :: Int -> Int -> Matrix Board -> (Int -> Int -> Matrix Board -> Weights) -> [Weights] -> [[Weights]] -> Matrix Weights
mapBoardAux r c mb f l wm
  -- r+1 and c+1 b/c when indexing a matrix with getElem its one indexed :O
  | r < dem && c < dem  = mapBoardAux r (c+1) mb f (f (r+1) (c+1) mb:l) wm
  | r < dem && c >= dem = mapBoardAux (r+1) 0 mb f [] (l:wm)
  | otherwise = fromLists wm where
  dem = nrows mb

-- | 'mapBoard' is a map transform over any Matrix 'Board'
-- where the function exposes (row -> column -> matrix 'Board' -> 'Weights')
-- 'mapBoard' will only take square matrices
mapBoard :: Matrix Board -> (Int -> Int -> Matrix Board -> Weights) -> Matrix Weights
mapBoard mb f = mapBoardAux 0 0 mb f [] []

-- | 'setWeights' analyses the state of the current Board and adjusts the weights accordingly
setWeights :: Matrix Board -> Matrix Weights
setWeights mb_t = mapBoard mb_t (\r c mb -> if mb!(r,c) == Piece Red || mb!(r,c) == Piece White then (0.0,0.0,1.0,0.0) else (0.0,0.0,0.0,0.0))

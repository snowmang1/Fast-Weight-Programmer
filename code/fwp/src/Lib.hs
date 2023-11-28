module Lib
    (
    someFunc,
    setBoard,
    setWeights,
    mapMat,
    move,
    MoveType (Forward, Backward, FTakeRight, FTakeLeft, BTakeLeft, BTakeRight),
    ) where

import Board (Board (Red, White, WhiteKing, RedKing, Empty))
import Data.Matrix

data MoveType = Forward | Backward | FTakeRight | FTakeLeft | BTakeLeft | BTakeRight

type Weights = (Float, Float, Float, Float) -- cap left, cap right, forward, backward

someFunc :: IO ()
someFunc = putStrLn "not ready for testing"

oscilateRow :: Int -> Int -> Bool -> [Board] -> [Board]
oscilateRow _ 0 _ ac = reverse ac
oscilateRow t c b ac = if mod c 2==t then oscilateRow t (c-1) b (if b then White:ac else Red:ac) else oscilateRow t (c-1) b (Empty:ac)
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

mapMatAux :: Int -> Int -> Matrix a -> (Int -> Int -> Matrix a -> b) -> [b] -> [[b]] -> Matrix b
mapMatAux r c mb f l wm
  | r < dem && c < dem  = mapMatAux r (c+1) mb f (f (r+1) (c+1) mb:l) wm
  | r < dem && c >= dem = mapMatAux (r+1) 0 mb f [] (l:wm)
  | otherwise = fromLists wm where
  dem = nrows mb

-- | 'mapMat' is a map transform over any 'Matrix' a that exposes the row,column # to f
-- where the function exposes (row -> column -> 'Matrix' a -> b)
-- 'mapMat' will currently only take square matrices
mapMat :: Matrix a -> (Int -> Int -> Matrix a -> b) -> Matrix b
mapMat mb f = mapMatAux 0 0 mb f [] []

-- | 'move' moves a single specified piece on the board with a 
-- move takes piece location, 'MoveType'
-- for captures right is column-1 and left is column+1 you can think of it as direction from whites
-- perspective.
move :: Int -> Int -> MoveType -> Matrix Board -> Maybe (Matrix Board)
move r c Forward m | x == Red && canMoveRed r c && getElem (r+1) c m == Empty =
                      Just (setElem Empty (r,c) (setElem x (r+1, c) m))
                   | x == White && canMoveWhite r c && getElem (r-1) c m == Empty =
                      Just (setElem Empty (r,c) (setElem x (r-1, c) m))
                   | otherwise = Nothing
                   where  x = getElem r c m
                          canMoveRed row col = row < 8 && col <= 8 && row >= 1 && col >= 1
                          canMoveWhite row col = row <= 8 && col <= 8 && row > 1 && col >= 1

move r c Backward m | x == RedKing && canMoveRed r c && getElem (r-1) c m == Empty =
                      Just (setElem Empty (r,c) (setElem x (r-1, c) m))
                    | x == WhiteKing && canMoveWhite r c && getElem (r+1) c m == Empty =
                      Just (setElem Empty (r,c) (setElem x (r+1, c) m))
                    | otherwise = Nothing
                    where x = getElem r c m
                          canMoveRed row col = row <= 8 && col <= 8 && row > 1 && col >= 1
                          canMoveWhite row col = row < 8 && col <= 8 && row >= 1 && col >= 1

move r c FTakeRight m | x == Red && canTake r c =
                        Just (setElem Empty (r,c) (setElem Empty (r+1, c-1) (setElem x (r+2,c-2) m)))
                      | x == White && canTake r c =
                        Just (setElem Empty (r,c) (setElem Empty (r-1, c+1) (setElem x (r-2,c+2) m)))
                      | otherwise = Nothing
                        where x = getElem r c m
                              canTake row col = row <= 8 && col <= 8 && row >= 1 && col >= 1 &&
                                (getElem (row+1) (col-1) m /= Empty || getElem (row+1) (col+1) m /= Empty)

move r c FTakeLeft m  | x == Red && canTake r c =
                        Just (setElem Empty (r,c) (setElem Empty (r+1, c+1) (setElem x (r+2,c+2) m)))
                      | x == White && canTake r c =
                        Just (setElem Empty (r,c) (setElem Empty (r-1, c-1) (setElem x (r-2,c-2) m)))
                      | otherwise = Nothing
                        where x = getElem r c m
                              canTake row col = row <= 8 && col <= 8 && row >= 1 && col >= 1 &&
                                (getElem (row+1) (col-1) m /= Empty || getElem (row+1) (col+1) m /= Empty)

move r c BTakeRight m | x == RedKing && canTake r c =
                        Just (setElem Empty (r,c) (setElem Empty (r-1, c-1) (setElem x (r-2,c-2) m)))
                      | x == WhiteKing && canTake r c =
                        Just (setElem Empty (r,c) (setElem Empty (r+1, c+1) (setElem x (r+2,c+2) m)))
                      | otherwise = Nothing
                        where x = getElem r c m
                              canTake row col = row <= 8 && col <= 8 && row >= 1 && col >= 1 &&
                                (getElem (row-1) (col-1) m /= Empty || getElem (row-1) (col+1) m /= Empty)

move r c BTakeLeft m | x == RedKing && canTake r c =
                        Just (setElem Empty (r,c) (setElem Empty (r-1, c+1) (setElem x (r-2,c+2) m)))
                     | x == WhiteKing && canTake r c =
                        Just (setElem Empty (r,c) (setElem Empty (r+1, c-1) (setElem x (r+2,c-2) m)))
                     | otherwise = Nothing
                        where x = getElem r c m
                              canTake row col = row <= 8 && col <= 8 && row >= 1 && col >= 1 &&
                                (getElem (row-1) (col-1) m /= Empty || getElem (row-1) (col+1) m /= Empty)

-- | 'setWeights' analyses the state of the current Board and adjusts the weights accordingly
setWeights :: Matrix Board -> Matrix Weights
setWeights mb_t = mapMat mb_t (\r c mb -> if mb!(r,c) == Red || mb!(r,c) == White then (0.0,0.0,1.0,0.0) else (0.0,0.0,0.0,0.0))

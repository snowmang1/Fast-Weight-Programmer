module Slow (
  analyzeBoard,
  mapWeights,
  analyzeWin
  ) where

import Board (Board (Red, White, WhiteKing, RedKing, Empty))
import Lib (Weights)
import Data.Matrix

-- | special map for analyzation step of slow matrix
-- at each step this map takes as context the current row col and gives it to f
-- f then outputs some Weight that will be set as the row col index of the Weight Matrix
mapWeights :: Matrix Board -> Matrix Weights -> (Int -> Int -> Matrix Board -> Weights) -> Matrix Weights
mapWeights = recMap 1 1 where
  recMap r c b w f
    | r <= dem && c < dem = recMap r (c+1) b (setElem (f r c b) (r,c) w) f
    | r <= dem && c == dem = recMap (r+1) 1 b (setElem (f r c b) (r,c) w) f
    | otherwise = w where
      dem = nrows b

bot :: (Int,Int,Matrix Board,Weights) -> Float
bot (row,col,mat,(_,_,_,_,_,_)) = if getElem (row+1) col mat == Empty then 1 else 0
top :: (Int,Int,Matrix Board,Weights) -> Float
top (row,col,mat,(_,_,_,_,_,_)) = if getElem (row-1) col mat == Empty then 1 else 0
topL :: (Int,Int,Matrix Board,Weights) -> Float
topL (row,col,mat,(_,_,_,_,_,_)) = if getElem (row-1) (col-1) mat == opposite row col mat then 1 else 0
  where opposite r c m = let cur = getElem r c m in if cur == Red || cur == RedKing then White else Red
topR :: (Int,Int,Matrix Board,Weights) -> Float
topR (row,col,mat,(_,_,_,_,_,_)) = if getElem (row-1) (col+1) mat == opposite row col mat then 1 else 0
  where opposite r c m = let cur = getElem r c m in if cur == Red || cur == RedKing then White else Red
botR :: (Int,Int,Matrix Board,Weights) -> Float
botR (row,col,mat,(_,_,_,_,_,_)) = if getElem (row+1) (col+1) mat == opposite row col mat then 1 else 0
  where opposite r c m = let cur = getElem r c m in if cur == Red || cur == RedKing then White else Red
botL :: (Int,Int,Matrix Board,Weights) -> Float
botL (row,col,mat,(_,_,_,_,_,_)) = if getElem (row+1) (col-1) mat == opposite row col mat then 1 else 0
  where opposite r c m = let cur = getElem r c m in if cur == Red || cur == RedKing then White else Red

-- \ both 'modulateK' & 'modulate' do the same thing with respect to whether or not something is a king
modulateK :: (Int,Int,Matrix Board,Weights) -> Weights
modulateK (_,_,_,x) = mod8 (raise x)
  where mod8 (for,bac,cfl,cfr,cbl,cbr) = let total = denom (for,bac,cfl,cfr,cbl,cbr) in (for/total,bac/total,cfl/total,cfr/total,cbl/total,cbr/total)
          where denom = zero_check . tot
                tot tup = first tup + sec tup + third tup + fourth tup + fifth tup + sixth tup
                  where first   (y,_,_,_,_,_) = y
                        sec     (_,y,_,_,_,_) = y
                        third   (_,_,y,_,_,_) = y
                        fourth  (_,_,_,y,_,_) = y
                        fifth   (_,_,_,_,y,_) = y
                        sixth   (_,_,_,_,_,y) = y
                zero_check i = if i /= 0 then i else 1
        raise = fb . takef . takeb
        fb (for,bac,cfl,cfr,cbl,cbr)
          | bac == 1 && for == 1  = (0,1,cfl,cfr,cbl,cbr)
          | bac == 0 && for == 1  = (1,0,cfl,cfr,cbl,cbr)
          | bac == 1 && for == 0  = (0,1,cfl,cfr,cbl,cbr)
          | otherwise             = (0,0,cfl,cbr,cbl,cbr)
        takef (for,bac,cfl,cfr,cbl,cbr)
          | cfl == 1 && cfr == 1  = (for, bac,5,5,cbl,cbr)
          | cfl == 0 && cfr == 1  = (for, bac,0,5,cbl,cbr)
          | cfl == 1 && cfr == 0  = (for, bac,5,0,cbl,cbr)
          | otherwise             = (for, bac,0,0,cbl,cbr)
        takeb  (for,bac,cfl,cfr,cbl,cbr)
          | cbl == 1 && cbr == 1  = (for,bac,cfl,cfr,5,5)
          | cbl == 0 && cbr == 1  = (for,bac,cfl,cfr,0,5)
          | cbl == 1 && cbr == 0  = (for,bac,cfl,cfr,5,0)
          | otherwise             = (for,bac,cfl,cfr,0,0)

-- \ this function will take a weight and if multiple things are possible it will
-- create an x<1 probability that will tend towards piece capture
modulate :: (Int,Int,Matrix Board,Weights) -> Weights
modulate (_,_,_,(for,_,cfl,cfr,_,_))
  | for == 1 && cfl == 1 && cfr == 1 = (0.20,0,0.40,0.40,0,0)
  | for == 0 && cfl == 1 && cfr == 1 = (0,0,0.50,0.50,0,0)
  | for == 1 && cfl == 0 && cfr == 1 = (0.25,0,0,0.75,0,0)
  | for == 0 && cfl == 0 && cfr == 1 = (0,0,0,1,0,0)
  | for == 1 && cfl == 1 && cfr == 0 = (0.25,0,0.75,0,0,0)
  | for == 0 && cfl == 1 && cfr == 0 = (0,0,1,0,0,0)
  | for == 1 && cfl == 0 && cfr == 0 = (1,0,0,0,0,0)
  | otherwise                        = (0,0,0,0,0,0)

-- | single window analysis. take a window and make decisions based on the center piece.
-- form opinion in the form of a set of weights, function designed for 'mapWeights'
-- utilizing a window around the given piece this modulates the weights useing 'moduleate'
-- & 'modulateK' fxns to eliminate impossible play and promote "better" play.
-- takes note of pieces that are trapped by the wall(s)
analyzeWin :: Int -> Int -> Matrix Board -> Weights
analyzeWin r c m
  | cur == RedKing   = modulateK (checkBoardR (r,c,m,(0,0,0,0,0,0)))
  | cur == WhiteKing = modulateK (checkBoardW (r,c,m,(0,0,0,0,0,0)))
  | cur == Red       = modulate (checkBoardR (r,c,m,(0,0,0,0,0,0)))
  | cur == White     = modulate (checkBoardW (r,c,m,(0,0,0,0,0,0)))
  | otherwise        = (0,0,0,0,0,0)
  where leftedge  = c == 1 && r <= nrows m
        rightedge = c == ncols m && r <= nrows m
        topedge   = c <= ncols m && r == 1
        botedge   = c <= ncols m && r == nrows m
        cur       = getElem r c m 
        checkBoardW t 
          | not leftedge && not rightedge && not topedge && not botedge = (r,c,m,(top t, bot t, topL t, topR t, botL t, botR t))
          | leftedge && not rightedge && not topedge && not botedge     = (r,c,m,(top t, bot t, 0, topR t, 0, botR t))
          | leftedge && not rightedge && topedge && not botedge         = (r,c,m,(0, bot t, 0, 0, 0, botR t))
          | leftedge && not rightedge && not topedge && botedge         = (r,c,m,(top t, 0, 0, topR t, 0, 0))
          | not leftedge && rightedge && not topedge && not botedge     = (r,c,m,(top t, bot t, topL t, 0, botL t, 0))
          | not leftedge && rightedge && topedge && not botedge         = (r,c,m,(0, bot t, 0, 0, botL t, 0))
          | not leftedge && rightedge && not topedge && botedge         = (r,c,m,(top t, 0, topL t, 0, 0, 0))
          | not leftedge && not rightedge && topedge && not botedge     = (r,c,m,(0, bot t, 0, 0, botL t, botR t))
          | not leftedge && not rightedge && not topedge && botedge     = (r,c,m,(top t, 0, topL t, topR t, 0, 0))
          | otherwise = undefined
        checkBoardR t
          | not leftedge && not rightedge && not topedge && not botedge = (r,c,m,(bot t, top t, botR t, botL t, topR t, topL t))
          | leftedge && not rightedge && not topedge && not botedge     = (r,c,m,(bot t, top t, botR t, 0, topR t, 0))
          | leftedge && not rightedge && topedge && not botedge         = (r,c,m,(bot t, 0, botR t, 0, 0, 0))
          | leftedge && not rightedge && not topedge && botedge         = (r,c,m,(0, top t, 0, 0, topR t, 0))
          | not leftedge && rightedge && not topedge && not botedge     = (r,c,m,(bot t, top t, 0, botL t, 0, topL t))
          | not leftedge && rightedge && not topedge && botedge         = (r,c,m,(0, top t, 0, 0, 0, topL t))
          | not leftedge && rightedge && topedge && not botedge         = (r,c,m,(bot t, 0, 0, 0, botL t, 0))
          | not leftedge && not rightedge && topedge && not botedge     = (r,c,m,(bot t, 0, botR t, botL t, 0, 0))
          | not leftedge && not rightedge && not topedge && botedge     = (r,c,m,(0, top t, 0, 0, topR t, topL t))
          | otherwise = undefined

-- | convience function serving the purpose of slow matrix analyzation
analyzeBoard :: Matrix Board -> Matrix Weights -> Matrix Weights
analyzeBoard m w = mapWeights m w analyzeWin

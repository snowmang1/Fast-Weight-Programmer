module Lib
    ( someFunc,
    setBoard,
    initWM
    ) where

import Data.Matrix

someFunc :: IO ()
someFunc = putStrLn "someFunc"

oscilateRow :: Int -> Int -> [Bool] -> [Bool]
oscilateRow _ 0 ac = reverse ac
oscilateRow t c ac = if (mod c 2==t) then oscilateRow t (c-1) (True:ac) else oscilateRow t (c-1) (False:ac)
setBoard :: Matrix Bool
setBoard =
  fromLists (red ++ mid ++ white) where
  red = oddtile:eventile:oddtile:[] where
    eventile = oscilateRow 0 8 []
    oddtile = oscilateRow 1 8 []
  mid = (replicate 2 (replicate 8 False))
  white = eventile:oddtile:eventile:[] where
    eventile = oscilateRow 0 8 []
    oddtile = oscilateRow 1 8 []

initWM :: Matrix Bool -> Matrix Float
initWM _ = fromLists (replicate 8 (replicate 8 (1/12)))

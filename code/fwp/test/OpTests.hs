module OpTests(
  testBoard,
  moveTests,
  captureTests,
  startBoard
) where

import Data.Matrix

import Test.Tasty
import Test.Tasty.HUnit

import qualified Lib

forwardRed :: Int -> Int -> Matrix Lib.Board
forwardRed r c = setElem Lib.Empty (r,c) (setElem Lib.Red (r+1,c) (fromLists testBoard))

forwardWhite :: Int -> Int -> Matrix Lib.Board
forwardWhite r c = setElem Lib.Empty (r,c) (setElem Lib.White (r-1,c) (fromLists testBoard))

backwardRed :: Int -> Int -> Matrix Lib.Board
backwardRed r c = setElem Lib.Empty (r,c) (setElem Lib.Red (r-1,c) (fromLists testBoard))

backwardWhite :: Int -> Int -> Matrix Lib.Board
backwardWhite r c = setElem Lib.Empty (r,c) (setElem Lib.White (r+1,c) (fromLists testBoard))

startBoard :: [[Lib.Board]]
startBoard = [[Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red],
            [ Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty],
            [Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red],
            [Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty],
            [Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty],
            [ Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty],
            [Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White],
            [ Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty]]

testBoard :: [[Lib.Board]]
testBoard = [[Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red],
            [ Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty],
            [Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red],
            [Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty,  Lib.White],
            [Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty,  Lib.Red, Lib.Empty],
            [ Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty],
            [Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White],
            [ Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty]]

moveTests :: TestTree
moveTests = testGroup "movement tests"
  [
  testCase "Red Forward 1 1" $
  Lib.move 1 1 Lib.Forward (fromLists testBoard) @?= Nothing,
  testCase "Red Forward 2 1" $
  Lib.move 2 1 Lib.Forward (fromLists testBoard) @?= Just (forwardRed 2 1),
  testCase "Red Forward 1 2" $
  Lib.move 1 2 Lib.Forward (fromLists testBoard) @?= Just (forwardRed 1 2),
  testCase "Red Forward 1 3" $
  Lib.move 1 3 Lib.Forward (fromLists testBoard) @?= Nothing,
  testCase "White Forward 8 1" $
  Lib.move 8 1 Lib.Forward (fromLists testBoard) @?= Just (forwardWhite 8 1),
  testCase "White Forward 8 2" $
  Lib.move 8 2 Lib.Forward (fromLists testBoard) @?= Nothing,

  -- collision tests
  testCase "White Forward collision" $
  Lib.move 6 7 Lib.Forward (fromLists testBoard) @?= Nothing,
  testCase "Red Forward collision" $
  Lib.move 3 8 Lib.Forward (fromLists testBoard) @?= Nothing,

  testCase "Red Backward 2 3" $
  Lib.move 2 3 Lib.Backward (fromLists testBoard) @?= Just (backwardRed 2 3),
  testCase "Red Backward 1 1" $
  Lib.move 1 1 Lib.Backward (fromLists testBoard) @?= Nothing,
  testCase "White Backward 6 3" $
  Lib.move 6 3 Lib.Backward (fromLists testBoard) @?= Just (backwardWhite 6 3),
  testCase "White Backward 8 1" $
  Lib.move 8 1 Lib.Backward (fromLists testBoard) @?= Nothing
  ]

captureBoard :: [[Lib.Board]]
captureBoard = [[Lib.Empty, Lib.Red, Lib.Empty, Lib.Red, Lib.Empty, Lib.Red, Lib.Empty, Lib.Red],
            [Lib.Red, Lib.Empty, Lib.White, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty],
            [Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.Red],
            [Lib.Empty, Lib.Empty,  Lib.White, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty],
            [Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty,  Lib.Red, Lib.Empty, Lib.Empty],
            [ Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty],
            [Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.Red, Lib.Empty,  Lib.White],
            [ Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty,  Lib.White, Lib.Empty]]

-- | row column direction-{0, 1, 2, 3} 0 for right and 1 for left. 2,3 for backward directions respective
captureRed:: Int -> Int -> Int -> Matrix Lib.Board
captureRed r c d | d == 0 = setElem Lib.Empty (r+1,c+1) (setElem Lib.Empty (r,c) (setElem Lib.Red (r+2,c+2) (fromLists captureBoard)))
                 | d == 1 = setElem Lib.Empty (r+1,c-1) (setElem Lib.Empty (r,c) (setElem Lib.Red (r+2,c-2) (fromLists captureBoard)))
                 | d == 2 = setElem Lib.Empty (r-1,c-1) (setElem Lib.Empty (r,c) (setElem Lib.Red (r-2,c-2) (fromLists captureBoard)))
                 | d == 3 = setElem Lib.Empty (r-1,c+1) (setElem Lib.Empty (r,c) (setElem Lib.Red (r-2,c+2) (fromLists captureBoard)))
                 | otherwise = fromLists captureBoard

-- | row column direction-{0, 1, 2, 3} 0 for right and 1 for left. 2,3 for backward directions respective
captureWhite:: Int -> Int -> Int -> Matrix Lib.Board
captureWhite r c d | d == 0 = setElem Lib.Empty (r-1,c+1) (setElem Lib.Empty (r,c) (setElem Lib.White (r-2,c+2) (fromLists captureBoard)))
                   | d == 1 = setElem Lib.Empty (r-1,c-1) (setElem Lib.Empty (r,c) (setElem Lib.White (r-2,c-2) (fromLists captureBoard)))
                   | d == 2 = setElem Lib.Empty (r+1,c+1) (setElem Lib.Empty (r,c) (setElem Lib.White (r+2,c+2) (fromLists captureBoard)))
                   | d == 3 = setElem Lib.Empty (r+1,c-1) (setElem Lib.Empty (r,c) (setElem Lib.White (r+2,c-2) (fromLists captureBoard)))
                   | otherwise = fromLists captureBoard

-- red right => - column, red left => + column
captureTests :: TestTree
captureTests = testGroup "capture tests"
  [
  testCase "Red 3 4 capture right" $
    Lib.move 3 4 Lib.FTakeRight (fromLists captureBoard) @?= Just (captureRed 3 4 1),
  testCase "Red 3 2 capture left" $
    Lib.move 3 2 Lib.FTakeLeft (fromLists captureBoard) @?= Just (captureRed 3 2 0),
  testCase "White 6 7 capture right" $
    Lib.move 6 5 Lib.FTakeRight (fromLists captureBoard) @?= Just (captureWhite 6 5 0),
  testCase "White 6 7 capture left" $
    Lib.move 6 7 Lib.FTakeLeft (fromLists captureBoard) @?= Just (captureWhite 6 7 1),

  -- backward tests
  testCase "Red 7 6 capture backward right" $
    Lib.move 7 6 Lib.BTakeRight (fromLists captureBoard) @?= Just (captureRed 7 6 2),
  testCase "Red 7 6 capture backward left" $
    Lib.move 7 6 Lib.BTakeLeft (fromLists captureBoard) @?= Just (captureRed 7 6 3),
  testCase "White 2 3 capture backward right" $
    Lib.move 2 3 Lib.BTakeRight (fromLists captureBoard) @?= Just (captureWhite 2 3 2),
  testCase "White 2 3 capture backward left" $
    Lib.move 2 3 Lib.BTakeLeft (fromLists captureBoard) @?= Just (captureWhite 2 3 3)

  -- collision tests
  ]

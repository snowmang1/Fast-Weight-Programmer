module OpTests(
  testBoard,
  moveTests,
  captureTests
) where

import Data.Matrix

import Test.Tasty
import Test.Tasty.HUnit

import qualified Lib

forwardRed :: Int -> Int -> Matrix Lib.Board
forwardRed r c = setElem Lib.Empty (r,c) (setElem (Lib.Piece Lib.Red) (r+1,c) (fromLists testBoard))

forwardWhite :: Int -> Int -> Matrix Lib.Board
forwardWhite r c = setElem Lib.Empty (r,c) (setElem (Lib.Piece Lib.White) (r-1,c) (fromLists testBoard))

backwardRed :: Int -> Int -> Matrix Lib.Board
backwardRed r c = setElem Lib.Empty (r,c) (setElem (Lib.Piece Lib.Red) (r-1,c) (fromLists testBoard))

backwardWhite :: Int -> Int -> Matrix Lib.Board
backwardWhite r c = setElem Lib.Empty (r,c) (setElem (Lib.Piece Lib.White) (r+1,c) (fromLists testBoard))

testBoard :: [[Lib.Board]]
testBoard = [[Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red],
            [Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty],
            [Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red],
            [Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Piece Lib.White],
            [Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty],
            [Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty],
            [Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White],
            [Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty]]

captureBoard :: [[Lib.Board]]
captureBoard = [[Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red],
            [Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty],
            [Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red, Lib.Empty, Lib.Piece Lib.Red],
            [Lib.Empty, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty],
            [Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty, Lib.Empty],
            [Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty],
            [Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White],
            [Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty, Lib.Piece Lib.White, Lib.Empty]]

{- FIX:
 - backward collision tests
 -}
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

captureRightForward :: Int -> Int -> Matrix Lib.Board
captureRightForward r c = setElem Lib.Empty (r+1,c+1) (setElem Lib.Empty (r,c) (setElem (Lib.Piece Lib.Red) (r+2,c+2) (fromLists captureBoard)))

-- red right => - column, red left => + column
captureTests :: TestTree
captureTests = testGroup "capture tests"
  [
  testCase "Red capture right" $
  Lib.move 3 2 Lib.FTakeRight (fromLists captureBoard) @?= Just (captureRightForward 3 2)
  ]

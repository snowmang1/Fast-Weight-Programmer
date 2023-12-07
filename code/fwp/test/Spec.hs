import Test.Tasty
import Test.Tasty.HUnit

import Data.Matrix
import qualified Lib
import qualified Board

import MvTests (moveTests, testBoard, captureTests, startBoard)
import SlowTests (slowTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [matrixTests, moveTests, captureTests, slowTests, winTests]

matrixTests :: TestTree
matrixTests = testGroup "Matrix type tests"
  [ testCase "Board setup" $
    toLists Lib.setBoard @?= startBoard,
    testCase "Map simple const test" $
    toLists (Lib.mapMat (fromLists testBoard) (\_ _ _ -> (1.0,0.0,0.0,0.0,0,0))) @?= [
                                              [(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0), --Red
                                              (1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0)],
                                              [(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),
                                              (1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0)],
                                              [(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),
                                              (1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0)],

                                              [(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0), --Middle
                                              (1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0)],
                                              [(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),
                                              (1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0)],

                                              [(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0), --White
                                              (1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0)],
                                              [(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),
                                              (1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0)],
                                              [(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),
                                              (1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0),(1.0,0.0,0.0,0.0,0,0)]
                                              ]
  ]

redWinBoard :: [[Board.Board]]
redWinBoard = [[Board.Empty,  Board.Red, Board.Empty,  Board.Red, Board.Empty,  Board.Red, Board.Empty,  Board.Red],
              [Board.Red, Board.Empty,  Board.Red, Board.Empty,  Board.Red, Board.Empty,  Board.Red, Board.Empty],
              [Board.Empty,  Board.Red, Board.Empty,  Board.Red, Board.Empty,  Board.Red, Board.Empty,  Board.Red],
              [Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty],
              [Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty],
              [Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty, Board.Empty],
              [Board.Empty,  Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty],
              [Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty, Board.Empty]]

whiteWinBoard :: [[Board.Board]]
whiteWinBoard=[[Board.Empty,  Board.White, Board.Empty,  Board.White, Board.Empty,  Board.White, Board.Empty,  Board.White],
              [Board.White, Board.Empty,  Board.White, Board.Empty,  Board.White, Board.Empty,  Board.White, Board.Empty],
              [Board.Empty,  Board.White, Board.Empty,  Board.White, Board.Empty,  Board.White, Board.Empty,  Board.White],
              [Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty],
              [Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty, Board.Empty],
              [Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty, Board.Empty],
              [Board.Empty,  Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty],
              [Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty, Board.Empty]]

midGameBoard :: [[Board.Board]]
midGameBoard = [[Board.Empty,   Board.Red, Board.Empty,   Board.Red,  Board.Empty,   Board.Red,  Board.Empty,    Board.Red],
                [Board.Red,   Board.Empty,   Board.Red, Board.Empty,    Board.Red, Board.Empty,    Board.Red,  Board.Empty],
                [Board.Empty,   Board.Red, Board.Empty,   Board.Red,  Board.Empty,   Board.Red,  Board.Empty,    Board.Red],
                [Board.Empty, Board.Empty, Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty,  Board.Empty],
                [Board.Empty, Board.Empty, Board.Empty, Board.Empty,   Board.Empty,Board.Empty,  Board.Empty,  Board.Empty],
                [Board.White, Board.Empty, Board.White, Board.Empty,   Board.White,Board.Empty,  Board.White,  Board.Empty],
                [Board.Empty, Board.White, Board.Empty, Board.White,   Board.Empty,Board.White,  Board.Empty,  Board.White],
                [Board.White, Board.Empty,  Board.White,Board.Empty,   Board.White,Board.Empty,  Board.White, Board.Empty]]

winTests :: TestTree
winTests = testGroup "tests for win condition"
  [
  testCase "Unfinished game" $
  Lib.winGame (fromLists midGameBoard) @?= Nothing,
  testCase "Red won game" $
  Lib.winGame (fromLists redWinBoard) @?= Just Board.Red,
  testCase "White won game" $
  Lib.winGame (fromLists whiteWinBoard) @?= Just Board.White
  ]

oneMoveBoard :: [[Board.Board]]
oneMoveBoard = [[Board.Empty,   Board.Red, Board.Empty,   Board.Red,  Board.Empty,   Board.Red,  Board.Empty,    Board.Red],
                [Board.Red,   Board.Empty,   Board.Red, Board.Empty,    Board.Red, Board.Empty,    Board.Red,  Board.Empty],
                [Board.Empty,   Board.Red, Board.Empty,   Board.Red,  Board.Empty,   Board.Red,  Board.Empty,    Board.Red],
                [Board.Empty, Board.Empty, Board.Empty, Board.Empty,  Board.Empty, Board.Empty,  Board.Empty,  Board.Empty],
                [Board.White, Board.Empty, Board.Empty, Board.Empty,   Board.Empty,Board.Empty,  Board.Empty,  Board.Empty],
                [Board.Empty, Board.Empty, Board.White, Board.Empty,   Board.White,Board.Empty,  Board.White,  Board.Empty],
                [Board.Empty, Board.White, Board.Empty, Board.White,   Board.Empty,Board.White,  Board.Empty,  Board.White],
                [Board.White, Board.Empty,  Board.White,Board.Empty,   Board.White,Board.Empty,  Board.White, Board.Empty]]

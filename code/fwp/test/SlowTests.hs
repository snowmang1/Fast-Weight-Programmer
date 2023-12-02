module SlowTests(
  slowTests,
) where

import Data.Matrix

import Test.Tasty
import Test.Tasty.HUnit

import qualified Lib
import qualified Board
import qualified Slow

testWindow1 :: Matrix Board.Board
testWindow1 = fromLists [[Board.Empty, Board.Empty, Board.Empty],
                        [Board.Empty, Board.White, Board.Empty],
                        [Board.Empty, Board.Empty, Board.Empty]]

testWindow2 :: Matrix Board.Board
testWindow2 = fromLists [[Board.Red, Board.Red, Board.Empty],
                        [Board.Empty, Board.White, Board.Empty],
                        [Board.Empty, Board.Empty, Board.Red]]

testWindow3 :: Matrix Board.Board
testWindow3 = fromLists [[Board.Red, Board.Red, Board.Empty],
                        [Board.Empty, Board.WhiteKing, Board.Empty],
                        [Board.Red, Board.Empty, Board.Red]]

testWindow4 :: Matrix Board.Board
testWindow4 = fromLists [[Board.Red, Board.Red, Board.Empty],
                        [Board.Empty, Board.Red, Board.Empty],
                        [Board.Empty, Board.White, Board.White]]

testWindow5 :: Matrix Board.Board
testWindow5 = fromLists [[Board.Red, Board.Red, Board.White],
                        [Board.Empty, Board.RedKing, Board.Empty],
                        [Board.White, Board.Empty, Board.White]]

testWindow6 :: Matrix Board.Board
testWindow6 = fromLists [[Board.Empty, Board.Empty, Board.Empty],
                        [Board.Red, Board.Empty, Board.Empty],
                        [Board.Empty, Board.Empty, Board.Empty]]

testWindow7 :: Matrix Board.Board
testWindow7 = fromLists [[Board.Empty, Board.Empty, Board.Empty],
                        [Board.Empty, Board.Empty, Board.White],
                        [Board.Empty, Board.Empty, Board.Empty]]

testWindow8 :: Matrix Board.Board
testWindow8 = fromLists [[Board.Empty, Board.Empty, Board.Empty],
                        [Board.Red, Board.Empty, Board.Empty],
                        [Board.Empty, Board.White, Board.Empty]]

testWindow9 :: Matrix Board.Board
testWindow9 = fromLists [[Board.Empty, Board.Red, Board.Red],
                        [Board.Empty, Board.Empty, Board.White],
                        [Board.Empty, Board.Empty, Board.Empty]]

testWindow10 :: Matrix Board.Board
testWindow10 = fromLists [[Board.Empty, Board.Empty, Board.Empty],
                        [Board.Empty, Board.Empty, Board.Red],
                        [Board.Empty, Board.Red, Board.Empty]]

testWindow11 :: Matrix Board.Board
testWindow11 = fromLists [[Board.Empty, Board.Red, Board.Red],
                        [Board.White, Board.Empty, Board.Empty],
                        [Board.Empty, Board.Empty, Board.Empty]]

testWindow12 :: Matrix Board.Board
testWindow12 = fromLists [[Board.Empty, Board.Red, Board.Red],
                        [Board.Empty, Board.Red, Board.Empty],
                        [Board.White, Board.Empty, Board.White]]

testWeights :: Matrix Lib.Weights
testWeights = fromLists [[(0,0,0,0,0,0), (0,0,0,0,0,0), (0,0,0,0,0,0)],
                        [(0,0,0,0,0,0), (0,0,0,0,0,0), (0,0,0,0,0,0)],
                        [(0,0,0,0,0,0), (0,0,0,0,0,0), (0,0,0,0,0,0)]]

testWeightBoard :: Matrix Lib.Weights
testWeightBoard = fromLists [[(0,0,0,0,0,0),(1,0,0,0,0,0),(0,0,0,0,0,0),(1,0,0,0,0,0),(0,0,0,0,0,0),(1,0,0,0,0,0),(0,0,0,0,0,0),(1,0,0,0,0,0)],
                            [(1,0,0,0,0,0),(0,0,0,0,0,0),(1,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(1,0,0,0,0,0),(0,0,0,0,0,0)],
                            [(0,0,0,0,0,0),(1,0,0,0,0,0),(0,0,0,0,0,0),(0.25,0,0.75,0,0,0),(0,0,0,0,0,0),(0.25,0,0,0.75,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0)],
                            [(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0.20,0,0.40,0.40,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0)],
                            [(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0.25,0,0,0.75,0,0)],
                            [(1,0,0,0,0,0),(0,0,0,0,0,0),(1,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0.25,0,0,0.75,0,0),(0,0,0,0,0,0)],
                            [(0,0,0,0,0,0),(1,0,0,0,0,0),(0,0,0,0,0,0),(1,0,0,0,0,0),(0,0,0,0,0,0),(1,0,0,0,0,0),(0,0,0,0,0,0),(1,0,0,0,0,0)],
                            [(1,0,0,0,0,0),(0,0,0,0,0,0),(1,0,0,0,0,0),(0,0,0,0,0,0),(0,1/11,0,0,5/11,5/11),(0,0,0,0,0,0),(1,0,0,0,0,0),(0,0,0,0,0,0)]]

testBoard :: Matrix Board.Board
testBoard = fromLists       [[Board.Empty,Board.Red,Board.WhiteKing,Board.Red,Board.Empty,Board.Red,Board.Empty,Board.Red],
                            [Board.Red,Board.Empty,Board.Red,Board.Empty,Board.Empty,Board.Empty,Board.Red,Board.Empty],
                            [Board.Empty,Board.Red,Board.Empty,Board.Red,Board.Empty,Board.Red,Board.Empty,Board.Empty],
                            [Board.Empty,Board.Empty,Board.Empty,Board.Empty,Board.White,Board.Empty,Board.Empty,Board.Empty],
                            [Board.Empty,Board.Empty,Board.Empty,Board.Empty,Board.Empty,Board.Empty,Board.Empty,Board.Red],
                            [Board.White,Board.Empty,Board.White,Board.Empty,Board.Empty,Board.Empty,Board.White,Board.Empty],
                            [Board.Empty,Board.White,Board.Empty,Board.White,Board.Empty,Board.White,Board.Empty,Board.White],
                            [Board.White,Board.Empty,Board.White,Board.Empty,Board.RedKing,Board.Empty,Board.White,Board.Empty]]

slowTests :: TestTree
slowTests = testGroup "slow matrix functionality testing"
  [
    testCase "mapWeights smoke test" $
    Slow.mapWeights testWindow1 testWeights (\_ _ _ -> (1,0,0,0,0,0)) @?= mapPos (\(_,_) _ -> (1.0,0,0,0,0,0)) testWeights,
    testCase "analysis step for single window White p=1" $
    Slow.analyzeWin 2 2 testWindow1 @?= (1,0,0,0,0,0),
    testCase "analysis step for single window White p=3" $
    Slow.analyzeWin 2 2 testWindow2 @?= (0,0,1,0,0,0),
    testCase "analysis step for single window WhiteKing p=4" $
    Slow.analyzeWin 2 2 testWindow3 @?= (0,1/16,5/16,0,5/16,5/16),
    testCase "analysis step for single window Red p=1" $
    Slow.analyzeWin 2 2 testWindow4 @?= (0,0,1,0,0,0),
    testCase "analysis step for single window RedKing p=4" $
    Slow.analyzeWin 2 2 testWindow5 @?= (1/16,0,5/16,5/16,5/16,0),
    testCase "analysis step for single window left side Red" $
    Slow.analyzeWin 2 1 testWindow6 @?= (1,0,0,0,0,0),
    testCase "analysis step for single window left side White" $
    Slow.analyzeWin 2 3 testWindow7 @?= (1,0,0,0,0,0),
    testCase "analysis step for single window left side Red take" $
    Slow.analyzeWin 2 1 testWindow8 @?= (0.25,0,0.75,0,0,0),
    testCase "analysis step for single window left side White take" $
    Slow.analyzeWin 2 3 testWindow9 @?= (0,0,1,0,0,0),
    testCase "analysis step for single window rigth side Red" $
    Slow.analyzeWin 2 3 testWindow10 @?= (1,0,0,0,0,0),
    testCase "analysis step for single window left side White" $
    Slow.analyzeWin 2 1 testWindow11 @?= (0.25,0,0,0.75,0,0),
    testCase "analysis step for single window Red testing modulate" $
    Slow.analyzeWin 2 2 testWindow12 @?= (0.20,0,0.40,0.40,0,0),
    testCase "total board analysis" $
    Slow.analyzeBoard testBoard testWeightBoard @?= testWeightBoard
  ]

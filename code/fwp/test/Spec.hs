import Test.Tasty
import Test.Tasty.HUnit

import qualified Lib
import Data.Matrix

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [matrixTests]

matrixTests :: TestTree
matrixTests= testGroup "Matrix type tests"
  [ testCase "Board setup" $
    toLists Lib.setBoard @?= [[False, True, False, True, False, True, False, True],
                                [True, False, True, False, True, False, True, False],
                                [False, True, False, True, False, True, False, True],
                                [False, False, False, False, False, False, False, False],
                                [False, False, False, False, False, False, False, False],
                                [True, False, True, False, True, False, True, False],
                                [False, True, False, True, False, True, False, True],
                                [True, False, True, False, True, False, True, False]],
    testCase "Fast matrix init" $
    toLists (Lib.initWM Lib.setBoard) @?= [[0,0,0,0,0,0,0,0],
                            [0,1/12,0,1/12,0,1/12,0,1/12],
                            [1/12,0,1/12,0,1/12,0,1/12,0],
                            [0,1/12,0,1/12,0,1/12,0,1/12],
                            [1/12,0,1/12,0,1/12,0,1/12,0],
                            [0,1/12,0,1/12,0,1/12,0,1/12],
                            [1/12,0,1/12,0,1/12,0,1/12,0],
                            [0,0,0,0,0,0,0,0]]
  ]

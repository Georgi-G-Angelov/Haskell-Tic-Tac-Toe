module Tests (Tests.main) where
import IC.TestSuite
import TicTacToe hiding (main)


gameOverTestCases
   = [ testBoard1 ==> True
     , testBoard2 ==> False
     , testBoard3 ==> True
     , emptyBoard 3 ==> False
     , testBoard6 ==> True
     , testBoard4 ==> True
     , testBoard5 ==> False
     ]

parsePositionTestCases
   = [
       ("0 2") ==> (Just (0,2))
     , ("0 -8") ==> (Just (0,-8))
     , ("-4 1") ==> (Just (-4,1))
     , ("0 %1") ==> (Nothing)
     , ("") ==> (Nothing)
     , ("1 2 3") ==> (Nothing)
     , ("3 3") ==> (Just (3,3))
     , ("-1 2") ==> (Just (-1,2))
     , ("3% 3") ==> (Nothing)
     , ("1o1 1") ==> (Nothing)
     ]

tryMoveTestCases
  = [
      (X,(0,0),testBoard2) ==> (Nothing)
    , (O,(-1,2),testBoard2) ==> (Nothing)
    , (O,(0,-1),testBoard2) ==> (Nothing)
    , (O,(1,1),testBoard2) ==> (Just ([Taken X,Empty,Empty,Taken O],2))
    , (O,(3,3),testBoard1) ==> (Just ([Taken O,Taken X,Empty,Taken O,Taken O,
                                Empty,Taken X,Taken X,Taken O,Empty,Empty,Taken
                                X,Taken O,Taken X,Empty,Taken O],4))
    , (X,(0,0),testBoard4) ==> (Nothing)
    , (O,(1,0),testBoard4) ==> (Just ([Taken O,Taken X,Taken O,
                                       Taken O,Taken O,Taken X,
                                       Taken O,Empty  ,Taken X],3))
    , (X,(2,1),testBoard4) ==> (Just ([Taken O,Taken X,Taken O,
                                       Empty  ,Taken O,Taken X,
                                       Taken O,Taken X,Taken X],3))
    , (X,(0,1),testBoard5) ==> (Just ([Taken O,Taken X,Taken O,
                                       Empty  ,Taken O,Taken X,
                                       Taken X,Empty  ,Taken X],3))
    , (O,(2,2),testBoard5) ==> (Nothing)
    ]

isFullTestCases
  = [
      (emptyBoard 1) ==> False
    , (emptyBoard 2) ==> False
    , (testBoard1)   ==> False
    , (testBoard6)   ==> True
    ]

emptyBoardTestCases
  = [
      1 ==> ([Empty],1)
    , 2 ==> ([Empty,Empty,Empty,Empty],2)
    ]

-- You can add your own test cases above

allTestCases
  = [
      TestCase "gameOver" (gameOver)
               gameOverTestCases
    , TestCase "parsePosition" (parsePosition)
               parsePositionTestCases
    , TestCase "tryMove" (uncurry3 tryMove)
               tryMoveTestCases
    , TestCase "isFull" (isFull)
               isFullTestCases
    , TestCase "emptyBoard" (emptyBoard)
               emptyBoardTestCases
    ]


runTests = mapM_ goTest allTestCases

main = runTests

module Tests.InitialBoardTests (allBoardTests,kingMoveTests) where

import Tests.TestSuite

allBoardTests = TestList [
    TestLabel "All Initial Moves Black" testIMB
    ,TestLabel "All Initial Moves White" testIMW
    ,TestLabel "Figure Count White" testFCW
    ,TestLabel "Figure Count Black" testFCB 
    ,TestLabel "No Check White" testNCW
    ,TestLabel "No Check Black" testNCB
     ]
-- 20 StartingMoves Black
testIMB = 20 ~=? length (validMoves initialBoard B)
-- 20 StartingMoves White 
testIMW = 20 ~=? length (validMoves initialBoard W)
-- 16 Starting Figures for White
testFCW = 16 ~=? length (piecesForPlayer initialBoard W)
-- 16 Starting Figures for Black
testFCB = 16 ~=? length (piecesForPlayer initialBoard B)
-- White doesn't start in Check 
testNCW = False ~=? check initialBoard W
-- Black doesn't start in Check 
testNCB = False ~=? check initialBoard B


kingMoveTests = TestList [
    TestLabel "SafeWhite" testSW,
    TestLabel "SafeBlack" testSB,
    TestLabel "MiddleWhite" testMW,
    TestLabel "MiddleBlack" testMB
    ]

testSW = 3 ~=? length ( validMoves safeKings W )

testSB = 3 ~=? length ( validMoves safeKings B )

mw = addKing [] (1,1) B
mw' = addKing mw (5,5) W 

testMW = 8 ~=? length ( validMoves mw' W )

mb = addKing [] (5,5) B
mb' = addKing mb (1,1) W 

testMB = 8 ~=? length ( validMoves mb' B)
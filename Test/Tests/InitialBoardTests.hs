module Tests.InitialBoardTests (allBoardTests) where

import Tests.TestSuite

allBoardTests = TestList [
    TestLabel "All Initial Moves Black" testInitialMovesBlack
    ,TestLabel "All Initial Moves White" testInitialMovesWhite
    ,TestLabel "Figure Count White" testInitialFiguresWhite
    ,TestLabel "Figure Count Black" testInitialFiguresBlack 
    ,TestLabel "No Check White" testInitialNoCheckWhite
    ,TestLabel "No Check Black" testInitialNoCheckBlack
     ]
-- 20 StartingMoves Black
testInitialMovesBlack = 20 ~=? length (validMoves initialBoard B)
-- 20 StartingMoves White 
testInitialMovesWhite = 20 ~=? length (validMoves initialBoard W)
-- 16 Starting Figures for White
testInitialFiguresWhite = 16 ~=? length (piecesForPlayer initialBoard W)
-- 16 Starting Figures for Black
testInitialFiguresBlack = 16 ~=? length (piecesForPlayer initialBoard B)
-- White doesn't start in Check 
testInitialNoCheckWhite = False ~=? check initialBoard W
-- Black doesn't start in Check 
testInitialNoCheckBlack = False ~=? check initialBoard B
module Tests.CoreMovementTests (allCoreMovementTests) where

import Tests.TestSuite

allCoreMovementTests = TestList [
    TestLabel "KingCoreMoves" allKingCoreMoves
    , TestLabel "KnightCoreMOves" allKnightCoreMoves
    , TestLabel "Lines Function" allLines 
    , TestLabel "StopAtNearest" allStopAt
    , TestLabel "OnBoard" allOnBoard
    ]

allKingCoreMoves = TestList [
    TestLabel "Free " testKF,
    TestLabel "Cornered" testKC,
    TestLabel "Cornered MoveFilter" testKCM
    ]

testKF = 8 ~=? length (kingMoves (5,5))
testKC = 8 ~=? length (kingMoves (8,8))
testKCM = 3 ~=? length (moveFilter [Chesspiece King (8,8) W] (kingMoves (8,8)))

allKnightCoreMoves = TestList [
    TestLabel "Free " testHF,
    TestLabel "Cornered" testHC,
    TestLabel "Cornered MoveFilter" testHCM
    ]

testHF = 8 ~=? length (knightMoves (5,5))
testHC = 8 ~=? length (knightMoves (8,8))
testHCM = 2 ~=? length (moveFilter [Chesspiece Knight (8,8) W] (knightMoves (8,8)))

allLines = TestList [
    TestLabel "Rights Count" testHL1,
    TestLabel "Lefts Count" testHL2,
    TestLabel "Ups Count" testVL1,
    TestLabel "Downs Count" testVL2,
    TestLabel "Rising Digs R Count" testRL1,
    TestLabel "Rising Digs L Count" testRL2,
    TestLabel "Falling Digs R Count" testFL1,
    TestLabel "Falling Digs L Count" testFL2,
    TestLabel "Lines do not have themselves in it I" testLN1,
    TestLabel "Lines do not have themselves in it II" testLN2
    ]

testHL1 = 8 ~=? length (rights (5,5))
testHL2 = 8 ~=? length (lefts (5,5))
testVL1 = 8 ~=? length (ups (5,5))
testVL2 = 8 ~=? length (downs (5,5))
testRL1 = 8 ~=? length (risingDigR (5,5))
testRL2 = 8 ~=? length (risingDigL (5,5))
testFL1 = 8 ~=? length (fallingDigR (5,5))
testFL2 = 8 ~=? length (fallingDigL (5,5))

testLN1 = False ~=? (5,5) `elem` (ups (5,5))
testLN2 = False ~=? (5,5) `elem` (fallingDigL (5,5))

allStopAt = TestList [
    TestLabel "Stopped" testS,
    TestLabel "Not Stopped " testNS,
    TestLabel "Stopped Late" testSL,
    TestLabel "Empty Line" testSEL
    ]

up11 = ups (1,1)

testS = 2 ~=? length (stopAt up11 (1,3))
testNS = 8 ~=? length (stopAt up11 (2,2))
testSL = 5 ~=? length (stopAt up11 (1,6))
testSEL = 0 ~=? length (stopAt [] (1,6))

allOnBoard = TestList [
    TestLabel "X/Y On Board" testXYOB,
    TestLabel "X not on Board" testXOB,
    TestLabel "Y not on Board" testYOB,
    TestLabel "X/Y not on Board" testXYNOB
    ]

testXYOB = True ~=? onBoard (3,3)
testXOB = False ~=? onBoard (3,10)
testYOB = False ~=? onBoard (30,3)
testXYNOB = False ~=? onBoard (30,10)
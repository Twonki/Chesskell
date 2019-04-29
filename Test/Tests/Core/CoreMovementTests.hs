module Tests.Core.CoreMovementTests (allCoreMovementTests) where

import Tests.TestSuite

allCoreMovementTests = TestList [
    TestLabel "KingCoreMoves testStopped" allKingCoreMoves
    , TestLabel "KnightCoreMoves testStopped" allKnightCoreMoves
    , TestLabel "Lines Function testStopped " allLines 
    , TestLabel "StopAtNearest testStopped" allStopAt
    , TestLabel "OnBoard testStopped" allOnBoard
    ]

allKingCoreMoves = TestList [
    TestLabel "Centered " testFreeKing,
    TestLabel "Cornered" testKingCorneredNoFilter,
    TestLabel "Cornered MoveFilter" testKingCorneredFiltered
    ]

testFreeKing = 8 ~=? length (kingMoves (5,5))
testKingCorneredNoFilter = 8 ~=? length (kingMoves (8,8))
testKingCorneredFiltered = 3 ~=? length (moveFilter [Chesspiece King (8,8) W] (kingMoves (8,8)))

allKnightCoreMoves = TestList [
    TestLabel "Centered " testFreeKnight,
    TestLabel "Cornered" testKnightCorneredNotFiltered,
    TestLabel "Cornered MoveFilter" testKnightCorneredFiltered
    ]

testFreeKnight = 8 ~=? length (knightMoves (5,5))
testKnightCorneredNotFiltered = 8 ~=? length (knightMoves (8,8))
testKnightCorneredFiltered = 2 ~=? length (moveFilter [Chesspiece Knight (8,8) W] (knightMoves (8,8)))

allLines = TestList [
    TestLabel "Rights Count" testRightsCount,
    TestLabel "Lefts Count" testLeftsCount,
    TestLabel "Ups Count" testUpsCount,
    TestLabel "Downs Count" testDownsCount,
    TestLabel "Rising Digs R Count" testRightRisingDigsCount,
    TestLabel "Rising Digs L Count" testLeftRisingDigsCount,
    TestLabel "Falling Digs R Count" testRightFallingDigsCount,
    TestLabel "Falling Digs L Count" testLeftFallingDigsCount,
    TestLabel "Lines do not have themselves in it I" testNoVerticalSelfcontainment,
    TestLabel "Lines do not have themselves in it II" testNoDiagonalSelfcontainment
    ]

testRightsCount = 8 ~=? length (rights (5,5))
testLeftsCount = 8 ~=? length (lefts (5,5))
testUpsCount = 8 ~=? length (ups (5,5))
testDownsCount = 8 ~=? length (downs (5,5))
testRightRisingDigsCount = 8 ~=? length (risingDigR (5,5))
testLeftRisingDigsCount = 8 ~=? length (risingDigL (5,5))
testRightFallingDigsCount = 8 ~=? length (fallingDigR (5,5))
testLeftFallingDigsCount = 8 ~=? length (fallingDigL (5,5))

testNoVerticalSelfcontainment = False ~=? (5,5) `elem` (ups (5,5))
testNoDiagonalSelfcontainment = False ~=? (5,5) `elem` (fallingDigL (5,5))

allStopAt = TestList [
    TestLabel "Stopped" testStopped,
    TestLabel "Not Stopped " testNotStopped,
    TestLabel "Stopped Late" testStoppedFarAway,
    TestLabel "Empty Line" testStoppedEmptyLine
    ]

up11 = ups (1,1)

testStopped = 2 ~=? length (stopAt up11 (1,3))
testNotStopped = 8 ~=? length (stopAt up11 (2,2))
testStoppedFarAway = 5 ~=? length (stopAt up11 (1,6))
testStoppedEmptyLine = 0 ~=? length (stopAt [] (1,6))

allOnBoard = TestList [
    TestLabel "X/Y On Board" testXYOnBoard,
    TestLabel "X not on Board" testXOnBoardYNot,
    TestLabel "Y not on Board" testYOnBoardXNot,
    TestLabel "X/Y not on Board" testYAndYNotOnBoard
    ]

testXYOnBoard = True ~=? onBoard (3,3)
testXOnBoardYNot = False ~=? onBoard (3,10)
testYOnBoardXNot = False ~=? onBoard (30,3)
testYAndYNotOnBoard = False ~=? onBoard (30,10)
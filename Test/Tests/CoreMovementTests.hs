module Tests.CoreMovementTests (allCoreMovementTests) where

import Tests.TestSuite
import Control.Monad(join)
import Data.Maybe(isNothing)

allCoreMovementTests = TestList [
    TestLabel "KingCoreMoves testStopped" allKingCoreMoves
    , TestLabel "KnightCoreMoves testStopped" allKnightCoreMoves
    , TestLabel "Lines Function testStopped " allLines 
    , TestLabel "StopAtNearest testStopped" allStopAt
    , TestLabel "OnBoard testStopped" allOnBoard
    , TestLabel "Missing Pieces Tests" allMissingPiecesTests
    , TestLabel "MoveTo Tests" allMoveToTests
    ]

allKingCoreMoves = TestList [
    TestLabel "Centered " testFreeKing,
    TestLabel "Cornered" testKingCorneredNoFilter,
    TestLabel "Cornered MoveFilter" testKingCorneredFiltered
    ]

testFreeKing = 8 ~=? length (kingMoves (5,5))
testKingCorneredNoFilter = 8 ~=? length (kingMoves (8,8))
testKingCorneredFiltered = 3 ~=? length filteredMoves'
    where 
        posmoves = kingMoves (8,8)
        singularBoard = [Chesspiece King (8,8) W] -- Board with only my Knight
        filteredMoves = (moveFilter singularBoard) <$> posmoves
        filteredMoves' = join filteredMoves

allKnightCoreMoves = TestList [
    TestLabel "Centered " testFreeKnight,
    TestLabel "Cornered" testKnightCorneredNotFiltered,
    TestLabel "Cornered MoveFilter" testKnightCorneredFiltered
    ]

testFreeKnight = 8 ~=? length (knightMoves (5,5))
testKnightCorneredNotFiltered = 8 ~=? length (knightMoves (8,8))
testKnightCorneredFiltered = 2 ~=? length filteredMoves'
    where 
        posmoves = (knightMoves (8,8))
        singularBoard = [Chesspiece Knight (8,8) W] -- Board with only my Knight
        filteredMoves = (moveFilter singularBoard) <$> posmoves
        filteredMoves' = join filteredMoves

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


allMissingPiecesTests = TestList [
    TestLabel "Empty Board White" testEmptyBoardWhiteMissingPieces
    ,TestLabel "Empty Board Black" testEmptyBoardBlackMissingPieces
    ,TestLabel "Full Board White" testFullBoardWhiteMissingPieces
    ,TestLabel "Full Board Black" testFullBoardBlackMissingPieces
    ,TestLabel "Only Queen + King White" testQueenAndKingWhiteMissingPieces
    ,TestLabel "Only Queen + King Black" testQueenAndKingBlackMissingPieces
    ,TestLabel "One Missing Tower" testOneMissingTowerMissingPieces
    ,TestLabel "Two Missing Towers" testTwoMissingTowerMissingPieces
    ]
-- There are 6 distinct figures on board
testEmptyBoardWhiteMissingPieces = 6 ~=? length ( missingPieces [] W)
testEmptyBoardBlackMissingPieces = 6 ~=? length ( missingPieces [] B)

testFullBoardWhiteMissingPieces = 0 ~=? length ( missingPieces initialBoard W )
testFullBoardBlackMissingPieces = 0 ~=? length ( missingPieces initialBoard B )

oneMissingTower = removeFigure initialBoard (1,8)
twoMissingTowers = removeFigure oneMissingTower (8,8)

-- There should be no difference if there is one or two towers missing
testOneMissingTowerMissingPieces =  1 ~=? length ( missingPieces oneMissingTower  W )
testTwoMissingTowerMissingPieces =  1 ~=? length ( missingPieces twoMissingTowers W )

testQueenAndKingWhiteMissingPieces = 4 ~=? length ( missingPieces (addQueen safeKings (5,5) W) W )
testQueenAndKingBlackMissingPieces = 4 ~=? length ( missingPieces (addQueen safeKings (5,5) B) B )


allMoveToTests = TestList [
    TestLabel "MoveTo on Free Field is Just" testFreeField
    ,TestLabel "MoveTo on Ally Field is Nothing" testMoveOntoAllyField
    ,TestLabel "MoveTo on Enemy Field is Just " testMoveOntoEnemyField
    ,TestLabel "MoveTo on Enemy Field removes Enemy" testMoveOntoEnemyRemoval
    ,TestLabel "MoveTo ignores Normal Piece Movement-Restrictions" testMoveToNoGameLogic1
    ,TestLabel "MoveTo ignores normal Board-Size Restrictions" testMoveToNoGameLogic2
    ]
-- A simple tower to perform these tests
tower = Chesspiece Tower (5,5) W
-- A Board with the single tower is my basecase
base = tower:[]

-- I can move here, it`s free
testFreeField = False ~=? isNothing (moveTo base tower (5,6))

-- A friendly piece is in the way
alliedField = addPawn base (5,6) W
-- I cannot move here
testMoveOntoAllyField = True ~=? isNothing (moveTo alliedField tower (5,6))

-- A enemy piece is in the way
enemyField = addPawn base (5,6) B
-- I can move here
testMoveOntoEnemyField = False ~=? isNothing (moveTo enemyField tower (5,6))
-- If i do, the pawn gets removed
testMoveOntoEnemyRemoval = 1 ~=? length ( fromJust' (moveTo enemyField tower (5,6)))

-- I can move even if tower cannot move there
testMoveToNoGameLogic1 = False ~=? isNothing (moveTo base tower (6,6))

-- I can move outside the field
testMoveToNoGameLogic2 = False ~=? isNothing (moveTo base tower (10,22))

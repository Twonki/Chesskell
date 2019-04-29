module Tests.CheckTests (checkTests,checkMateTests,checkLimitedMoveTests) where

import Tests.TestSuite

checkTests = TestList [
    TestLabel "No Initial Check White" testNoInitialCheckWhite
    ,TestLabel "No Initial Check Black" testNoInitialCheckBlack
    ,TestLabel "Far Away Aggressor produces Check" testFarAggressorCheck
    ,TestLabel "Close Aggressor produces Check" testCloseAggressorCheck
    ,TestLabel "Double-Aggressors produce Check" testDoubleAggressorCheck
    ]

-- Reminder: Safekings means WhiteKing on 1/1 and BlackKing on 8/8

testNoInitialCheckWhite = False ~=? check safeKings W
testNoInitialCheckBlack = False ~=? check safeKings B

testFarAggressorCheck = True  ~=? check (addTower safeKings (7,1) B) W
testCloseAggressorCheck = True  ~=? check (addTower safeKings (2,1) B) W

doubleAggressors = addTower (addTower safeKings (2,1) B) (1,7) B
testDoubleAggressorCheck = True  ~=? check doubleAggressors W

checkMateTests = TestList [
    TestLabel "No Intial Checkmate White" testNoInitialCheckMateWhite
    ,TestLabel "No Initial Checkmate Black" testNoInitialCheckMateBlack
    ,TestLabel "White King is in Checkmate" testCheckMateWhite
    ,TestLabel "White King is in Checkmate" testCheckMateBlack
    ,TestLabel "King can move out of Check" testCheckMateKingCanMoveOut
    ,TestLabel "King can kill Aggressor" testCheckMateKingCanRemoveAggressor
    ,TestLabel "Player can ScapeGoat a Piece" testCheckMateScapegoatOther   
    ]

testNoInitialCheckMateWhite = False ~=? checkmate safeKings W
testNoInitialCheckMateBlack = False ~=? checkmate safeKings B

checkmateWhite = addTower (addTower safeKings (1,5) B) (2,5) B
testCheckMateWhite = True ~=? checkmate checkmateWhite W

checkmateBlack = addTower (addTower safeKings (7,5) W) (8,5) W
testCheckMateBlack = True ~=? checkmate checkmateBlack B

-- King can simply move out of check
testCheckMateKingCanMoveOut = False ~=? checkmate (addTower safeKings (8,5) W) B 

-- Another Unit can kill the Agressor, resolving check
-- A Black tower can kill the White tower producing check
killableAggressor= addTower checkmateBlack (8,4) B
testCheckMateKingCanRemoveAggressor = False ~=? checkmate killableAggressor B

-- take checkmateBlack from above
-- A tower can move in to safe the king 
scapegoatPiece = addTower checkmateBlack (2,7) B
testCheckMateScapegoatOther = False ~=? checkmate scapegoatPiece B

checkLimitedMoveTests = TestList[
    TestLabel "Cannot move King into Check Horizontal" testHorizontalCheckBlockedMoves
    ,TestLabel "Cannot move King into Check Vertical" testVerticalCheckBlockedMoves
    ,TestLabel "King can strike Aggressor" testAttackMovesStillWork
    ,TestLabel "Aggressor is Covered" testAggressorIsCovered
    ,TestLabel "Aggressor is Covered and Checkmates" testCoveredAggressorCheckmates
    ]

hoizontalCheckBlocked = addTower safeKings (8,5) W
testHorizontalCheckBlockedMoves = 2 ~=? length (validMoves hoizontalCheckBlocked B) 
verticalCheckBlocked = addTower safeKings (6,8) W
testVerticalCheckBlockedMoves = 2 ~=? length (validMoves verticalCheckBlocked B) 

strikableAggressor = (addTower safeKings (8,7) W)
-- The King can either move away or strike the tower, however i cannot move diagonally as it would mean another check
testAttackMovesStillWork = 1 ~=? length (validMoves strikableAggressor B)

-- A Queen Guards the Agressing Tower
coveredAggressor = (addQueen strikableAggressor (7,6) W)
-- The King cannot move onto the Tower
testAggressorIsCovered = 0 ~=? length (validMoves coveredAggressor B)
-- That means he is in checkmate
testCoveredAggressorCheckmates = True ~=? checkmate coveredAggressor B

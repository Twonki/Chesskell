module Tests.GameTests (allGameTests) where

import Tests.TestSuite

allGameTests = TestList [
     TestLabel "Move-Tests" allMoveTests
    ,TestLabel "InitialState is InitialBoard + White Player" testInitialGameState
    ,TestLabel "Pickup Tests" allPickupTests
    ,TestLabel "Intitial Board Tests" allBoardTests
    ,TestLabel "Fair Metrics Tests" allFairMetricTests
    ]

-- =======================================================
-- Movement Tests 
-- Feeding Moves into the Gamestate
-- If the Movement Fails, the GameState will not Change
-- ======================================================
allMoveTests = TestList [
     TestLabel "Valid White Turn" testValidWhiteTurn
    ,TestLabel "Valid White Knight Opener" testValidWhiteKnightOpener
    ,TestLabel "Valid Black Turn" testValidBlackTurn
    ,TestLabel "Valid Black Knight Opener" testValidBlackKnightOpener
    ,TestLabel "Invalid White Move - No Figure" testInvalidFigureWhiteTurn
    ,TestLabel "Invalid White Move - Not Reachable" testInvalidMoveWhiteTurn
    ,TestLabel "Invalid White Move - Blocked" testInvalidMoveWhiteTurn2
    ,TestLabel "Invalid Black Move - No Figure" testInvalidFigureBlackTurn
    ,TestLabel "Invalid Black Move - Not Reachable" testInvalidMoveBlackTurn
    ,TestLabel "Invalid Black Move - Blocked" testInvalidMoveBlackTurn2
    ,TestLabel "Invalid Move - Out of Board" testInvalidMoveWhiteOutofBoard
    ,TestLabel "Invalid Move - Out of Board through Board" testInvalidMoveWhiteOutofBoardThroughBoard
    ,TestLabel "Invalid Move - Out of Board - Negative Coords" testInvalidMoveWhiteOutofBoardNegativePos
    ]

blackState = (initialBoard,B)
whiteState = (initialBoard,W)
-- I move a single pawn
validWhiteMove = ((2,7),(2,5))
testValidWhiteTurn = True ~=? whiteState /= (movePiece whiteState validWhiteMove)

-- I Move a White Knight as Opener
validWhiteKnightOpener = ((2,8),(3,6))
testValidWhiteKnightOpener = 
    True ~=? 
        whiteState /= (movePiece whiteState validWhiteKnightOpener)

-- I move a single pawn
validBlackMove = ((2,2),(2,4))
testValidBlackTurn = 
    True ~=? blackState /= (movePiece blackState validBlackMove)

-- I move a Black Knight as Opener
validBlackKnightOpener = ((2,1),(3,3))
testValidBlackKnightOpener = 
    True ~=? 
        blackState /= (movePiece blackState validBlackKnightOpener)

-- There is no Figure on this tile
invalidFigureWhiteMove = ((3,3),(5,2))
testInvalidFigureWhiteTurn = 
    True ~=? 
        whiteState == (movePiece whiteState invalidFigureWhiteMove)

-- There is a Figure, but it cannot move like that
invalidMoveWhiteMove = ((2,7),(2,3))
testInvalidMoveWhiteTurn =
    True ~=? 
        whiteState == (movePiece whiteState invalidMoveWhiteMove)

-- There is a Figure, but it cannot move like that
invalidMoveWhiteMove2 = ((1,8),(4,4))
testInvalidMoveWhiteTurn2 =
    True ~=? 
        whiteState == (movePiece whiteState invalidMoveWhiteMove2)


-- There is no Figure on this tile
invalidFigureBlackMove = ((3,3),(5,2))
testInvalidFigureBlackTurn = 
    True ~=? 
        blackState == (movePiece blackState invalidFigureBlackMove)

-- There is a Figure, but it cannot move like that
invalidMoveBlackMove = ((2,2),(2,6))
testInvalidMoveBlackTurn =
    True ~=? 
        blackState == (movePiece blackState invalidMoveBlackMove)

-- There is a Figure, but it cannot move like that
invalidMoveBlackMove2 = ((8,1),(4,4))
testInvalidMoveBlackTurn2 =
    True ~=? 
        blackState == (movePiece blackState invalidMoveBlackMove2)

-- I want to move the white Tower out of the Board
invalidMoveWhiteOutofBoard = ((1,8),(1,9))
testInvalidMoveWhiteOutofBoard =
    True ~=? 
        whiteState == (movePiece whiteState invalidMoveWhiteOutofBoard)

-- I want to move the white Tower out of the Board and pass many Figures on the way (Way should be blocked)
invalidMoveWhiteOutofBoardThroughBoard = ((1,8),(9,8))
testInvalidMoveWhiteOutofBoardThroughBoard =
    True ~=? 
        whiteState == (movePiece whiteState invalidMoveWhiteOutofBoardThroughBoard)

-- I want to move the white tower out of the board on a negative position        
invalidMoveWhiteOutofBoardNegativePos = ((1,8),(-4,8))
testInvalidMoveWhiteOutofBoardNegativePos =
    True ~=? 
        whiteState == (movePiece whiteState invalidMoveWhiteOutofBoardNegativePos)

-- Have my State as i expect it to be 
testInitialGameState =
    True ~=? 
        whiteState == initialGameState

-- ===========================================================
-- PickUp Tests
-- These methods check the function "CanPickup"
-- The Methods are aware of the GameState
-- ===========================================================
allPickupTests = TestList [
     TestLabel "Can Pickup White" testValidPickupWhite
    ,TestLabel "Can PickUp Black" testValidPickupWhite
    ,TestLabel "Cannot Pickup Enemy" testInvalidPickupFigure
    ,TestLabel "Cannot PickUp Empty Tile" testInvalidPickupNoFigure
    ,TestLabel "Cannot Pickup Out of Board Y Too Far" testInvalidPickupOutOfBoardYOut
    ,TestLabel "Cannot Pickup Out of Board X Negative" testInvalidPickupOutOfBoardNegative
    ,TestLabel "Cannot Pickup Out of Board XY Too Far" testInvalidPickupOutOfBoardBothOut
    ,TestLabel "Cannot Pickup Out of Board XY Negative" testInvalidPickupOutOfBoardBothNegative
    ]

-- White Pawn picked up by White player
testValidPickupWhite = 
    True ~=? canPickUp (2,7) whiteState

-- Black Pawn picked up by Black Player
testValidPickupBlack = 
    True ~=? canPickUp (2,2) blackState 

-- Black Pawn picked up by White Player    
testInvalidPickupFigure = 
    False ~=? canPickUp (2,2) whiteState 

-- No Figure on This Place - White Player    
testInvalidPickupNoFigure = 
    False ~=? canPickUp (5,5) whiteState

-- Out of Board - White Player    
testInvalidPickupOutOfBoardYOut = 
    False ~=? canPickUp (5,9) whiteState

-- Out of Board - White Player    
testInvalidPickupOutOfBoardNegative = 
    False ~=? canPickUp (-2,5) whiteState

-- Out of Board - White Player 
testInvalidPickupOutOfBoardBothOut = 
    False ~=? canPickUp (15,9) whiteState

-- Out of Board - White Player    
testInvalidPickupOutOfBoardBothNegative = 
    False ~=? canPickUp (-2,-5) whiteState

    
-- ================================================================
-- Initial Board Tests
-- Check some Attributes i Expect the start-board to have
-- ================================================================
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

-- =======================================================================
-- Fair Metrics Tests 
-- =======================================================================
allFairMetricTests = TestList [
    TestLabel "Fair Simple" testFairSimpleMetric
    ,TestLabel "Fair RatedSimple" testFairRatedSimpleMetric
    ,TestLabel "Fair Agility" testFairAgilityMetric
    ]

-- All Tests Check whether both parties have the same metric in initial state

testFairSimpleMetric = True ~=? (simple initialBoard W) == (simple initialBoard B)
testFairRatedSimpleMetric = True ~=? (ratedSimple initialBoard W) == (ratedSimple initialBoard B)
testFairAgilityMetric = True ~=? (agility initialBoard W) == (agility initialBoard B)
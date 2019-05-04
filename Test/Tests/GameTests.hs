module Tests.GameTests (allGameTests) where

import Tests.TestSuite

allGameTests = TestList [
    TestLabel "Valid White Turn" testValidWhiteTurn
    ,TestLabel "Valid Black Turn" testValidBlackTurn
    ,TestLabel "Invalid White Move - No Figure" testInvalidFigureWhiteTurn
    ,TestLabel "Invalid White Move - Not Reachable" testInvalidMoveWhiteTurn
    ,TestLabel "Invalid White Move - Blocked" testInvalidMoveWhiteTurn2
    ,TestLabel "InitialState is InitialBoard + White Player" testInitialGameState
    ,TestLabel "Can Pickup White" testValidPickupWhite
    ,TestLabel "Can PickUp Black" testValidPickupWhite
    ,TestLabel "Cannot Pickup Enemy" testInvalidPickupFigure
    ,TestLabel "Cannot PickUp Empty Tile" testInvalidPickupNoFigure
    ,TestLabel "Intitial Board Tests" allBoardTests
    ,TestLabel "Fair Metrics Tests" allFairMetricTests
    ]

whiteState = (initialBoard,W)
-- I move a single pawn
validWhiteMove = ((2,7),(2,5))

testValidWhiteTurn = True ~=? whiteState /= (movePiece whiteState validWhiteMove)

blackState = (initialBoard,B)
-- I move a single pawn
validBlackMove = ((2,2),(2,4))

testValidBlackTurn = True ~=? blackState /= (movePiece blackState validBlackMove)

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


testInitialGameState =
    True ~=? 
        whiteState == initialGameState

testValidPickupWhite = 
    True ~=? canPickUp (2,7) whiteState 

testValidPickupBlack = 
    True ~=? canPickUp (2,2) blackState 

testInvalidPickupFigure = 
    False ~=? canPickUp (2,2) whiteState 

testInvalidPickupNoFigure = 
    False ~=? canPickUp (5,5) whiteState


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



allFairMetricTests = TestList [
    TestLabel "Fair Simple" testFairSimpleMetric
    ,TestLabel "Fair RatedSimple" testFairRatedSimpleMetric
    ,TestLabel "Fair Agility" testFairAgilityMetric
    ]

testFairSimpleMetric = True ~=? (simple initialBoard W) == (simple initialBoard B)
testFairRatedSimpleMetric = True ~=? (ratedSimple initialBoard W) == (ratedSimple initialBoard B)
testFairAgilityMetric = True ~=? (agility initialBoard W) == (agility initialBoard B)
module Tests.Game.GameTests (allGameTests) where

import Tests.TestSuite

allGameTests = TestList [
    TestLabel "Valid White Turn" testValidWhiteTurn
    ,TestLabel "Valid Black Turn" testValidBlackTurn
    ,TestLabel "Invalid White Move - No Figure" testInvalidFigureWhiteTurn
    ,TestLabel "Invalid White Move - Not Reachable" testInvalidMoveWhiteTurn
    ,TestLabel "InitialState is InitialBoard + White Player" testInitialGameState
    ,TestLabel "Can Pickup White" testValidPickupWhite
    ,TestLabel "Can PickUp Black" testValidPickupWhite
    ,TestLabel "Cannot Pickup Enemy" testInvalidPickupFigure
    ,TestLabel "Cannot PickUp Empty Tile" testInvalidPickupNoFigure
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
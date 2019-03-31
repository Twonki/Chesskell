module Tests.PawnTests (allPawnTests) where
    
import Tests.TestSuite

allPawnTests = TestList [
   TestLabel "Initial Moves BlackPawn" testIMBP
   ,TestLabel "Initial Moves WhitePawn" testIMWP
   ]

testIMBP = 2 ~=? countMoves (moves bor bp)
    where 
        wp = ChessFigure Pawn (2,7) W 
        bp = ChessFigure Pawn (7,2) B
        bor = wp:bp:safeKings

testIMWP = 2 ~=? countMoves (moves bor wp)
    where 
        wp = ChessFigure Pawn (2,7) W 
        bp = ChessFigure Pawn (7,2) B
        bor = wp:bp:safeKings




addPawn :: Board -> Pos -> Player -> Board 
addPawn b p c = (ChessFigure Pawn p c):b 
module Tests.PawnTests (allPawnTests) where
    
import Tests.TestSuite

allPawnTests = TestList [
   TestLabel  "Initial Moves BlackPawn" testIMBP
   ,TestLabel "Initial Moves WhitePawn" testIMWP
   ,TestLabel "Strike  Moves BlackPawn" testSMBP
   ,TestLabel "Strike  Moves WhitePawn" testSMWP
   ,TestLabel "Strike  Moves BlackPawn II" testSMBP2
   ,TestLabel "Strike  Moves WhitePawn II" testSMWP2
   ,TestLabel "Blocked Moves BlackPawn" testBMBP
   ,TestLabel "Blocked Moves WhitePawn" testBMWP
   ,TestLabel "Blocked Moves Ally BlackPawn" testBMABP
   ,TestLabel "Blocked Moves Ally WhitePawn" testBMAWP
   ,TestLabel "Initial Move  Blocked BlackPawn" testIMBBP
   ,TestLabel "Initial Move  Blocked WhitePawn" testIMBWP
   ]

-- One Step + One Wide Step
testIMBP = 2 ~=? countMoves (moves bor bp)
    where 
        wp = Chesspiece Pawn (2,7) W 
        bp = Chesspiece Pawn (7,2) B
        bor = wp:bp:safeKings

-- One Step + One Wide Step
testIMWP = 2 ~=? countMoves (moves bor wp)
    where 
        wp = Chesspiece Pawn (2,7) W 
        bp = Chesspiece Pawn (7,2) B
        bor = wp:bp:safeKings

-- One Step + One Strike Step
testSMBP = 2 ~=? countMoves (moves bor bp)
    where 
        wp = Chesspiece Pawn (6,6) W 
        bp = Chesspiece Pawn (5,5) B
        bor = wp:bp:safeKings

-- One Step + One Strike Step        
testSMWP = 2 ~=? countMoves (moves bor wp)
    where 
        wp = Chesspiece Pawn (6,6) W 
        bp = Chesspiece Pawn (5,5) B
        bor = wp:bp:safeKings

-- One Step + two Strike Steps    
testSMBP2 = 3 ~=? countMoves (moves bor bp)
    where 
        wp = Chesspiece Pawn (6,6) W 
        wp2 = Chesspiece Pawn (4,6) W 
        bp = Chesspiece Pawn (5,5) B
        bor = wp:bp:wp2:safeKings

-- One Step + two Strike Step        
testSMWP2 = 3 ~=? countMoves (moves bor wp)
    where 
        wp = Chesspiece Pawn (6,6) W 
        bp = Chesspiece Pawn (5,5) B
        bp2 = Chesspiece Pawn (7,5) B
        bor = wp:bp:bp2:safeKings

-- No Steps - Pawn is blocked by enemy pawn
testBMBP = 0 ~=? countMoves (moves bor bp)
    where 
        wp = Chesspiece Pawn (5,6) W 
        bp = Chesspiece Pawn (5,5) B
        bor = wp:bp:safeKings

-- No Steps - Pawn is blocked by enemy pawn
testBMWP = 0 ~=? countMoves (moves bor wp)
    where 
        wp = Chesspiece Pawn (5,6) W 
        bp = Chesspiece Pawn (5,5) B
        bor = wp:bp:safeKings

-- No Steps - Pawn is blocked by ally
testBMAWP = 0 ~=? countMoves (moves bor wp)
    where 
        wp = Chesspiece Pawn (5,6) W 
        wp2 = Chesspiece Pawn (5,5) W
        bor = wp:wp2:safeKings

-- No Steps - Pawn is blocked by ally
testBMABP = 0 ~=? countMoves (moves bor bp)
    where 
        bp2 = Chesspiece Pawn (5,6) B 
        bp = Chesspiece Pawn (5,5) B
        bor = bp:bp2:safeKings

-- One Step - Wide Step is blocked by another ally piece
testIMBBP = 1 ~=? countMoves (moves bor bp)
    where 
        bp2 = Chesspiece Pawn (5,4) B 
        bp = Chesspiece Pawn (5,2) B
        bor = bp:bp2:safeKings

-- One Step - Wide Step is blocked by another ally piece
testIMBWP = 1 ~=? countMoves (moves bor wp)
    where 
        wp = Chesspiece Pawn (5,7) W 
        wp2 = Chesspiece Pawn (5,5) W
        bor = wp:wp2:safeKings
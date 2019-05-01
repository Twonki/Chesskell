module Tests.Movement.PawnTests (allPawnTests) where
    
import Tests.TestSuite

allPawnTests = TestList [
   TestLabel  "Initial Moves BlackPawn" testInitialMovesBlackPawn
   ,TestLabel "Initial Moves WhitePawn" testInitialMovesWhitePawn
   ,TestLabel "Strike  Moves BlackPawn" testStrikeMovesBlackPawn
   ,TestLabel "Strike  Moves WhitePawn" testStrikeMovesWhitePawn
   ,TestLabel "Strike  Moves BlackPawn II" testDoubleStrikeMovesBlackPawn
   ,TestLabel "Strike  Moves WhitePawn II" testDoubleStrikeMovesWhitePawn
   ,TestLabel "Blocked Moves BlackPawn" testBlockedBlackPawn
   ,TestLabel "Blocked Moves WhitePawn" testBlockedWhitePawn
   ,TestLabel "Blocked Moves Ally BlackPawn" testBlockedByAllyBlackPawn
   ,TestLabel "Blocked Moves Ally WhitePawn" testBlockedByAllyWhitePawn
   ,TestLabel "Initial Move  Blocked BlackPawn" testInitialMoveBlockedBlackPawn
   ,TestLabel "Initial Move  Blocked WhitePawn" testInitialMoveBlockedWhitePawn
   ]

-- One Step + One Wide Step
testInitialMovesBlackPawn = 2 ~=? countMoves (moves bor bp)
    where 
        wp = Chesspiece Pawn (2,7) W 
        bp = Chesspiece Pawn (7,2) B
        bor = wp:bp:safeKings

-- One Step + One Wide Step
testInitialMovesWhitePawn = 2 ~=? countMoves (moves bor wp)
    where 
        wp = Chesspiece Pawn (2,7) W 
        bp = Chesspiece Pawn (7,2) B
        bor = wp:bp:safeKings

-- One Step + One Strike Step
testStrikeMovesBlackPawn = 2 ~=? countMoves (moves bor bp)
    where 
        wp = Chesspiece Pawn (6,6) W 
        bp = Chesspiece Pawn (5,5) B
        bor = wp:bp:safeKings

-- One Step + One Strike Step        
testStrikeMovesWhitePawn = 2 ~=? countMoves (moves bor wp)
    where 
        wp = Chesspiece Pawn (6,6) W 
        bp = Chesspiece Pawn (5,5) B
        bor = wp:bp:safeKings

-- One Step + two Strike Steps    
testDoubleStrikeMovesBlackPawn = 3 ~=? countMoves (moves bor bp)
    where 
        wp = Chesspiece Pawn (6,6) W 
        wp2 = Chesspiece Pawn (4,6) W 
        bp = Chesspiece Pawn (5,5) B
        bor = wp:bp:wp2:safeKings

-- One Step + two Strike Step        
testDoubleStrikeMovesWhitePawn = 3 ~=? countMoves (moves bor wp)
    where 
        wp = Chesspiece Pawn (6,6) W 
        bp = Chesspiece Pawn (5,5) B
        bp2 = Chesspiece Pawn (7,5) B
        bor = wp:bp:bp2:safeKings

-- No Steps - Pawn is blocked by enemy pawn
testBlockedBlackPawn = 0 ~=? countMoves (moves bor bp)
    where 
        wp = Chesspiece Pawn (5,6) W 
        bp = Chesspiece Pawn (5,5) B
        bor = wp:bp:safeKings

-- No Steps - Pawn is blocked by enemy pawn
testBlockedWhitePawn = 0 ~=? countMoves (moves bor wp)
    where 
        wp = Chesspiece Pawn (5,6) W 
        bp = Chesspiece Pawn (5,5) B
        bor = wp:bp:safeKings

-- No Steps - Pawn is blocked by ally
testBlockedByAllyWhitePawn = 0 ~=? countMoves (moves bor wp)
    where 
        wp = Chesspiece Pawn (5,6) W 
        wp2 = Chesspiece Pawn (5,5) W
        bor = wp:wp2:safeKings

-- No Steps - Pawn is blocked by ally
testBlockedByAllyBlackPawn = 0 ~=? countMoves (moves bor bp)
    where 
        bp2 = Chesspiece Pawn (5,6) B 
        bp = Chesspiece Pawn (5,5) B
        bor = bp:bp2:safeKings

-- One Step - Wide Step is blocked by another ally piece
testInitialMoveBlockedBlackPawn = 1 ~=? countMoves (moves bor bp)
    where 
        bp2 = Chesspiece Pawn (5,4) B 
        bp = Chesspiece Pawn (5,2) B
        bor = bp:bp2:safeKings

-- One Step - Wide Step is blocked by another ally piece
testInitialMoveBlockedWhitePawn = 1 ~=? countMoves (moves bor wp)
    where 
        wp = Chesspiece Pawn (5,7) W 
        wp2 = Chesspiece Pawn (5,5) W
        bor = wp:wp2:safeKings
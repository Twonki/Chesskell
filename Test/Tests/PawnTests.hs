module Tests.PawnTests (allPawnTests) where
    
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
   ,TestLabel "Pawn Replacement Tests" allPawnReplacementTests
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


allPawnReplacementTests = TestList [
    TestLabel "Replace with all pieces missing" testAllMissingReplace,
    TestLabel "PieceCount stays same after Replacement" testPieceCountStaysSame,
    TestLabel "All Missing - Double Finishers" testAllMissingReplaceWithDoubleFinishers,
    TestLabel "PieceCount Replacement Double Finishers" testPieceCountStaysSameWithDoubleFinishers,
    TestLabel "No Replacing" testNoReplacerMoves,
    TestLabel "PieceCount No Replacing" testNoReplacerMovesPiececount,
    TestLabel "Test White replacePawn (not moves)" testWhiteReplacer,
    TestLabel "Test Black replacePawn (not moves)" testBlackReplacer,
    TestLabel "Test White replacePawn (not moves) PieceCount" testWhiteReplacerPieceCount,
    TestLabel "Test Black replacePawn (not moves) PieceCount" testBlackReplacerPieceCount
    ]

-- There are all pieces missing on the board (except for Kings)
-- The Replacement will be able to replace into any Piece
allmissing = addPawn safeKings (4,2) W
-- Expted Value: 
-- 3 Moves from King 
-- 4 Replacements as Knight, Bishop, Queen, Tower 
-- 1 Staying as Pawn
testAllMissingReplace = 8 ~=? length ( validMoves allmissing  W )
-- There will never be more than 3 Figures if i replace one
testPieceCountStaysSame = True ~=? all (3 == ) (map length (validMoves allmissing W))

-- now i check with two pawns which can possibly finish
allmissingDoubleFinishers = addPawn allmissing (5,2) W
-- Expted Value: 
-- 3 Moves from King 
-- 4 Replacements as Knight, Bishop, Queen, Tower 
-- 4 Addition Replacements
-- 2 Staying as Pawns
testAllMissingReplaceWithDoubleFinishers = 13 ~=? length ( validMoves allmissingDoubleFinishers  W )
-- There will never be more than 4 Figures if i replace one
testPieceCountStaysSameWithDoubleFinishers = True ~=? all (4 ==) (map length ( validMoves allmissingDoubleFinishers  W ))

-- Test the AntiThesis, if i cannot replace everything behaves normal
noreplacers = addPawn safeKings (4,3) W
-- Expted Value: 
-- 3 Moves from King 
-- 1 Move from Pawn
testNoReplacerMoves = 4 ~=? length ( validMoves noreplacers  W )
-- There will never be more than 3 Figures if i replace one
testNoReplacerMovesPiececount = True ~=? all (3 ==) (map length ( validMoves noreplacers  W ))


finisherWhite = Chesspiece Pawn (5,8) W
finisherBoardWhite = finisherWhite : safeKings

testWhiteReplacer = 5 ~=? length (replacePawn finisherBoardWhite finisherWhite)
testWhiteReplacerPieceCount = True ~=? all (3==) (length <$> options)
    where options = replacePawn finisherBoardWhite finisherWhite


finisherBlack = Chesspiece Pawn (5,1) B
finisherBoardBlack = finisherBlack : safeKings

testBlackReplacer = 5 ~=? length (replacePawn finisherBoardBlack finisherBlack)
testBlackReplacerPieceCount = True ~=? all (3==) (length <$> options)
    where options = replacePawn finisherBoardBlack finisherBlack
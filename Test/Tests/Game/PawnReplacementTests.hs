module Tests.Game.PawnReplacementTests (allPawnReplacementTests) where
    
import Tests.TestSuite

allPawnReplacementTests = TestList [
    TestLabel "Replace with all pieces missing" testAllMissingReplace,
    TestLabel "PieceCount stays same after Replacement" testPieceCountStaysSame,
    TestLabel "All Missing - Double Finishers" testAllMissingReplaceWithDoubleFinishers,
    TestLabel "PieceCount Replacement Double Finishers" testPieceCountStaysSameWithDoubleFinishers,
    TestLabel "No Replacing" testNoReplacerMoves,
    TestLabel "PieceCount No Replacing" testNoReplacerMovesPiececount
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
testPieceCountStaysSame = True ~=? (all ((==) 3) $ map (length) ( validMoves allmissing  W ))

-- now i check with two pawns which can possibly finish
allmissingDoubleFinishers = addPawn allmissing (5,2) W
-- Expted Value: 
-- 3 Moves from King 
-- 4 Replacements as Knight, Bishop, Queen, Tower 
-- 4 Addition Replacements
-- 2 Staying as Pawns
testAllMissingReplaceWithDoubleFinishers = 13 ~=? length ( validMoves allmissingDoubleFinishers  W )
-- There will never be more than 4 Figures if i replace one
testPieceCountStaysSameWithDoubleFinishers = True ~=? (all ((==) 4) $ map (length) ( validMoves allmissingDoubleFinishers  W ))

-- Test the AntiThesis, if i cannot replace everything behaves normal
noreplacers = addPawn safeKings (4,3) W
-- Expted Value: 
-- 3 Moves from King 
-- 1 Move from Pawn
testNoReplacerMoves = 4 ~=? length ( validMoves noreplacers  W )
-- There will never be more than 3 Figures if i replace one
testNoReplacerMovesPiececount = True ~=? (all ((==) 3) $ map (length) ( validMoves noreplacers  W ))

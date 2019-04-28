module Tests.PawnReplacementTests (allPawnReplacementTests) where
    
import Tests.TestSuite

allPawnReplacementTests = TestList [
    TestLabel "All Missing" testAM,
    TestLabel "PieceCount Replacement" testPCR,
    TestLabel "All Missing - Double Finishers" testAMDF,
    TestLabel "PieceCount Replacement Double Finishers" testPCRDF
    ]

am = addPawn safeKings (4,2) W
-- Expted Value: 
-- 3 Moves from King 
-- 4 Replacements as Knight, Bishop, Queen, Tower 
-- 1 Staying as Pawn
testAM = 8 ~=? length ( validMoves am  W )
-- There will never be more than 3 Figures if i replace one
testPCR = True ~=? (all ((==) 3) $ map (length) ( validMoves am  W ))

amDF = addPawn am (5,2) W
-- Expted Value: 
-- 3 Moves from King 
-- 4 Replacements as Knight, Bishop, Queen, Tower 
-- 4 Addition Replacements
-- 2 Staying as Pawns
testAMDF = 13 ~=? length ( validMoves amDF  W )
-- There will never be more than 4 Figures if i replace one
testPCRDF = True ~=? (all ((==) 4) $ map (length) ( validMoves amDF  W ))

module Tests.MovementTests (allMovementTests) where

import Tests.TestSuite

allMovementTests = TestList [
    TestLabel "KingMoves" allKingMoves
    , TestLabel "QueenMoves" allQueenMoves
    , TestLabel "KnightMoves" allKnightMoves
    , TestLabel "TowerMoves" allTowerMoves
    , TestLabel "BishopMoves" allBishopMoves
    -- PawnMoves are separate!
 ]


-- =======================================
-- ========== King =======================
-- =======================================
allKingMoves = TestList [
    TestLabel "SafeWhite" testSW,
    TestLabel "SafeBlack" testSB,
    TestLabel "MiddleWhite" testMW,
    TestLabel "MiddleBlack" testMB,
    TestLabel "Blocked by Friend" testKiBF,
    TestLabel "Blocked by Enemy" testKiBE 
    ]

testSW = 3 ~=? length ( validMoves safeKings W )

testSB = 3 ~=? length ( validMoves safeKings B )

mw = addKing [] (1,1) B
mw' = addKing mw (5,5) W 

testMW = 8 ~=? length ( validMoves mw' W )

mb = addKing [] (5,5) B
mb' = addKing mb (1,1) W 

testMB = 8 ~=? length ( validMoves mb' B)

kibf = addPawn mw' (6,6) W 
-- 7 Moves for King 
-- 1 Move for Pawn
testKiBF = 8 ~=? length ( validMoves kibf W)

kibe = addPawn mw' (6,6) B 
-- 7 Moves for King 
-- 1 Move onto enemy Pawn
testKiBE = 8 ~=? length ( validMoves kibe W)

-- =======================================
-- ========== Queen ======================
-- =======================================
allQueenMoves = TestList [
    TestLabel "All Free" testQAF
    ,TestLabel "Blocked by Friend" testQBBF
    ,TestLabel "Blocked by Straight Close Enemy" testQBBE
    ,TestLabel "Blocked by Straight Far Enemy" testQBBFE
    ,TestLabel "Blocked by Straight Far Far Enemy" testQBBFFE
    ,TestLabel "Blocked by Dig close Enemy" testQBBDCE
    ,TestLabel "Blocked by Dig Far Enemy" testQBBDFE
    ,TestLabel "At Edge of Board" testQAEOB
 ]

qaf = addQueen safeKings (5,5) W 
-- 3 Moves for King 
-- 14 Tower Moves for Queen
-- 12 Bishop Moves for Queen (On this Position!) 
testQAF = (3+14+12) ~=? length (validMoves qaf W)

qbbf = addPawn qaf (5,6) W 
-- 3 Moves for King
-- 1 Move for Pawn
-- 22 Moves for Queen
testQBBF = (3+1+22) ~=? length (validMoves qbbf W)

qbbe = addPawn qaf (5,6) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 23 Moves for Queen
testQBBE = (3+1+23) ~=? length (validMoves qbbe W)

qbbfe = addPawn qaf (5,7) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 24 Moves for Queen
testQBBFE = (3+1+24) ~=? length (validMoves qbbfe W)
-- Pawn is at the Edge, I can fully move
qbbffe = addPawn qaf (5,8) B 
-- 3 Moves for King
-- 26 Core Queen Moves
testQBBFFE = (3+26) ~=? length (validMoves qbbffe W)


qbbdce = addPawn qaf (6,6) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 23 Moves for Queen
testQBBDCE = (3+1+23) ~=? length (validMoves qbbdce W)

qbbdfe = addPawn qaf (7,7) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 24 Moves for Queen
testQBBDFE = (3+1+24) ~=? length (validMoves qbbdfe W)


qaeob = addQueen safeKings (5,8) W 
-- 3 Moves for King 
-- 21 Moves for Queen 
testQAEOB = 24 ~=? length (validMoves qaeob W)

-- =======================================
-- ========== Knight =====================
-- =======================================
allKnightMoves = TestList [
    TestLabel "All Free" testKAF
    ,TestLabel "Blocked by Friend" testKBBF
    ,TestLabel "Blocked by Enemy" testKBBE
    ,TestLabel "At Edge of Board" testKAEOB
 ]

kaf = addKnight safeKings (5,5) W 
-- 3 Moves for King 
-- 8 Moves for Knight 
testKAF = (3+8) ~=? length (validMoves kaf W)

kbbf = addPawn kaf (6,3) W 
-- 3 Moves for King 
-- 1 Move for Pawn 
-- 7 Moves for Knight
testKBBF = (3+1+7) ~=? length (validMoves kbbf W)

kbbe = addPawn kaf (6,3) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 7 Moves for Knight
testKBBE = (3+1+7) ~=? length (validMoves kbbe W)

kaeob = addKnight safeKings (5,8) W 
-- 3 Moves for King 
-- 4 Moves for Knight 
testKAEOB = (3+4) ~=? length (validMoves kaeob W)
    
-- =======================================
-- ========== Bishop =====================
-- =======================================
allBishopMoves = TestList [
    TestLabel "All Free" testBAF
    ,TestLabel "Blocked by Friend" testBBBF
    ,TestLabel "Blocked by Close Enemy" testBBBCE
    ,TestLabel "Blocked by Far Enemy" testBBBFE
    ,TestLabel "At Edge of Board" testBAEOB
 ]

baf = addBishop safeKings (5,5) W 
-- 3 Moves for King 
-- 12 Moves for Bishop 
testBAF = 15 ~=? length (validMoves baf W)

bbbf = addPawn baf (5,6) W 
-- 3 Moves for King 
-- 1 Move for Pawn 
-- 11 Moves for Bishop
testBBBF = 15 ~=? length (validMoves bbbf W)

bbbce = addPawn baf (5,6) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 11 Moves for Bishop
testBBBCE = 15 ~=? length (validMoves bbbce W)

bbbfe = addPawn baf (6,7) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 12 Moves for Bishop
testBBBFE = 16 ~=? length (validMoves bbbfe W)

baeob = addBishop safeKings (5,8) W 
-- 3 Moves for King 
-- 7 Moves for Bishop 
testBAEOB = 10 ~=? length (validMoves baeob W)
        
-- =======================================
-- ========== Tower ======================
-- =======================================
allTowerMoves = TestList [
    TestLabel "All Free" testTAF
    ,TestLabel "Blocked by Friend" testTBBF
    ,TestLabel "Blocked by Close Enemy" testTBBCE
    ,TestLabel "Blocked by Far Enemy" testTBBFE
    ,TestLabel "Blocked by Far Enemy Other Direction" testTBBFE2
    ,TestLabel "Blocked by Far Far Enemy" testTBBFFE
    ,TestLabel "At Edge of Board" testTAEOB
 ]

taf = addTower safeKings (5,5) W 
-- 3 Moves for King
-- A tower can move the whole board of 8 pieces with nothing in the way, but not his own field
-- Same for Y 
-- Resulting in (2x7) Moves
-- X Moves for Tower 
testTAF = (3+7+7) ~=? length (validMoves taf W)

tbbf = addPawn taf (5,6) W 
-- 3 Moves for King 
-- 1 Move for Pawn 
-- All Tower Moves minus the 4 fields blocked by pawn 
testTBBF = (3+1+14-4) ~=? length (validMoves tbbf W)

tbbce = addPawn taf (5,6) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- All Tower Moves minus the 3 fields blocked by pawn 
testTBBCE = (3+1+14-3) ~=? length (validMoves tbbce W)

tbbfe = addPawn taf (5,7) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- All Tower Moves minus the 2 fields blocked by pawn 
testTBBFE = (3+1+14-2) ~=? length (validMoves tbbfe W)

tbbfe2 = addPawn taf (5,3) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- All Tower Moves minus the 2 fields blocked by pawn 
testTBBFE2 = (3+1+14-2) ~=? length (validMoves tbbfe2 W)

-- Enemy is on the edge, i can move normal
tbbffe = addPawn taf (5,8) B 
-- 3 Moves for King
-- 14 base moves for Tower
testTBBFFE = (3+14) ~=? length (validMoves tbbffe W)

taeob = addTower safeKings (5,8) W 
-- 3 Moves for King 
-- 14 Moves for Tower  
testTAEOB = (3+7+7) ~=? length (validMoves taeob W)
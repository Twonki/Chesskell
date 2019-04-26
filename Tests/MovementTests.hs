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
    TestLabel "MiddleBlack" testMB
    ]

testSW = 3 ~=? length ( validMoves safeKings W )

testSB = 3 ~=? length ( validMoves safeKings B )

mw = addKing [] (1,1) B
mw' = addKing mw (5,5) W 

testMW = 8 ~=? length ( validMoves mw' W )

mb = addKing [] (5,5) B
mb' = addKing mb (1,1) W 

testMB = 8 ~=? length ( validMoves mb' B)


-- =======================================
-- ========== Queen ======================
-- =======================================
allQueenMoves = TestList [
    TestLabel "All Free" testQAF
    ,TestLabel "Blocked by Friend" testQBBF
    ,TestLabel "Blocked by Enemy" testQBBE
    ,TestLabel "At Edge of Board" testQAEOB
 ]

qaf = addQueen safeKings (5,5) W 
-- 3 Moves for King 
-- X Moves for Queen 
testQAF = 20 ~=? length (validMoves qaf W)

qbbf = addPawn qaf (5,6) W 
-- 3 Moves for King 
-- 1 Move for Pawn 
-- Y Moves for Queen
testQBBF = 20 ~=? length (validMoves qbbf W)

qbbe = addPawn qaf (5,6) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- Y Moves for Queen
testQBBE = 20 ~=? length (validMoves qbbe W)

qaeob = addQueen safeKings (5,8) W 
-- 3 Moves for King 
-- Z Moves for Queen 
testQAEOB = 15 ~=? length (validMoves qaeob W)

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
-- X Moves for Knight 
testKAF = 20 ~=? length (validMoves kaf W)

kbbf = addPawn kaf (5,6) W 
-- 3 Moves for King 
-- 1 Move for Pawn 
-- Y Moves for Knight
testKBBF = 20 ~=? length (validMoves kbbf W)

kbbe = addPawn kaf (5,6) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- Y Moves for Knight
testKBBE = 20 ~=? length (validMoves kbbe W)

kaeob = addKnight safeKings (5,8) W 
-- 3 Moves for King 
-- Z Moves for Knight 
testKAEOB = 15 ~=? length (validMoves kaeob W)
    
-- =======================================
-- ========== Bishop =====================
-- =======================================
allBishopMoves = TestList [
    TestLabel "All Free" testBAF
    ,TestLabel "Blocked by Friend" testBBBF
    ,TestLabel "Blocked by Enemy" testBBBE
    ,TestLabel "At Edge of Board" testBAEOB
 ]

baf = addBishop safeKings (5,5) W 
-- 3 Moves for King 
-- X Moves for Bishop 
testBAF = 20 ~=? length (validMoves baf W)

bbbf = addPawn baf (5,6) W 
-- 3 Moves for King 
-- 1 Move for Pawn 
-- Y Moves for Bishop
testBBBF = 20 ~=? length (validMoves bbbf W)

bbbe = addPawn baf (5,6) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- Y Moves for Bishop
testBBBE = 20 ~=? length (validMoves bbbe W)

baeob = addBishop safeKings (5,8) W 
-- 3 Moves for King 
-- Z Moves for Bishop 
testBAEOB = 15 ~=? length (validMoves baeob W)
        
-- =======================================
-- ========== Tower ======================
-- =======================================
allTowerMoves = TestList [
    TestLabel "All Free" testTAF
    ,TestLabel "Blocked by Friend" testTBBF
    ,TestLabel "Blocked by Enemy" testTBBE
    ,TestLabel "At Edge of Board" testTAEOB
 ]

taf = addTower safeKings (5,5) W 
-- 3 Moves for King 
-- X Moves for Tower 
testTAF = 20 ~=? length (validMoves taf W)

tbbf = addPawn taf (5,6) W 
-- 3 Moves for King 
-- 1 Move for Pawn 
-- Y Moves for Tower
testTBBF = 20 ~=? length (validMoves tbbf W)

tbbe = addPawn taf (5,6) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- Y Moves for Tower
testTBBE = 20 ~=? length (validMoves tbbe W)

taeob = addTower safeKings (5,8) W 
-- 3 Moves for King 
-- Z Moves for Tower 
testTAEOB = 15 ~=? length (validMoves taeob W)
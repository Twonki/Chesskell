module Tests.CoreMovementTests (allCoreMovementTests) where

import Tests.TestSuite

allCoreMovementTests = TestList [
    TestLabel "KingCoreMoves" allKingCoreMoves
    , TestLabel "QueenCoreMoves" allQueenCoreMoves
    , TestLabel "KnightCoreMoves" allKnightCoreMoves
    , TestLabel "TowerCoreMoves" allTowerCoreMoves
    , TestLabel "BishopCoreMoves" allBishopCoreMoves
    -- PawnMoves are separate!
    ]


-- =======================================
-- ========== King =======================
-- =======================================
allKingMoves = TestList [
    TestLabel "SafeWhite" testSW,
]

-- =======================================
-- ========== Queen ======================
-- =======================================


-- =======================================
-- ========== Knight =====================
-- =======================================


-- =======================================
-- ========== Bishop =====================
-- =======================================

        
-- =======================================
-- ========== Tower ======================
-- =======================================

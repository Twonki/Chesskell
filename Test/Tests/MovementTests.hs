module Tests.MovementTests (allMovementTests) where

import Tests.TestSuite

allMovementTests = TestList [
    TestLabel "KingMove Tests" allKingMoves
    , TestLabel "QueenMove Tests" allQueenMoves
    , TestLabel "KnightMove Tests" allKnightMoves
    , TestLabel "TowerMoves Tests" allTowerMoves
    , TestLabel "BishopMove Tests" allBishopMoves
    -- PawnMoves are separate!
 ]

-- =======================================
-- ========== King =======================
-- =======================================
allKingMoves = TestList [
    TestLabel "Corner Moves White King" testCorneredWhiteKingMoves,
    TestLabel "Corner Moves Black King" testCorneredBlackKingMoves,
    TestLabel "Moves central White King" testCentralWhiteKingMoves,
    TestLabel "Moves central Black King" testCentralBlackKingMoves,
    TestLabel "King is Blocked by Ally" testKingBlockedByAllyMoves,
    TestLabel "King is Blocked by Enemy" testKingBlockedByEnemyMoves 
    ]
-- I keep these separate because they are important for other tests
-- If I misscount here, I will also have false tests somewhere else
-- When these work, I know that some more complex methods do not fail at the kings movement
testCorneredWhiteKingMoves = 3 ~=? length ( validMoves safeKings W )
testCorneredBlackKingMoves = 3 ~=? length ( validMoves safeKings B )

corneredBlackKing = addKing [] (1,1) B
centralWhiteKing = addKing corneredBlackKing (5,5) W 

testCentralWhiteKingMoves = 8 ~=? length ( validMoves centralWhiteKing W )

centralBlackKing = addKing [] (5,5) B
corneredWhiteKing = addKing centralBlackKing (1,1) W 

testCentralBlackKingMoves = 8 ~=? length ( validMoves corneredWhiteKing B)

kingBlockedByFriend = addPawn centralWhiteKing (6,6) W 
-- 7 Moves for King 
-- 1 Move for Pawn
testKingBlockedByAllyMoves = 8 ~=? length ( validMoves kingBlockedByFriend W)

kingBlockedByEnemy = addPawn centralWhiteKing (6,6) B 
-- 7 Moves for King 
-- 1 Move onto enemy Pawn
testKingBlockedByEnemyMoves = 8 ~=? length ( validMoves kingBlockedByEnemy W)

-- =======================================
-- ========== Queen ======================
-- =======================================
allQueenMoves = TestList [
    TestLabel "All Free" testQueenAllFreeMoves
    ,TestLabel "Blocked by Straight Close Friend" testQueenBlockedByCloseFriend
    ,TestLabel "Blocked by Straight Far Friend" testQueenBlockedByFarFriend
    ,TestLabel "Blocked by Straight Close Enemy" testQueenBlockedByCloseEnemy
    ,TestLabel "Blocked by Straight Far Enemy" testQueenBlockedByFarEnemy
    ,TestLabel "Blocked by Straight Far Far Enemy" testQueenBlockedByFarerEnemy
    ,TestLabel "Blocked by Dig close Enemy" testQueenBlockedDiagonalByCloseEnemy
    ,TestLabel "Blocked by Dig far Enemy" testQueenBlockedDiagonalByFarEnemy
    ,TestLabel "Blocked by Dig close Friend" testQueenBlockedDiagonalByCloseFriend
    ,TestLabel "Blocked by Dig far Friend" testQueenBlockedDiagonalByFarFriend
    ,TestLabel "At Edge of Board" testQueenAtTheEdge
 ]

queenAllFree = addQueen safeKings (5,5) W 
-- 3 Moves for King 
-- 14 Tower Moves for Queen
-- 12 Bishop Moves for Queen (On this Position!) 
testQueenAllFreeMoves = (3+14+12) ~=? length (validMoves queenAllFree W)

queenBlockedByCloseFriend = addPawn queenAllFree (5,6) W 
-- 3 Moves for King
-- 1 Move for Pawn
-- 22 Moves for Queen
testQueenBlockedByCloseFriend = (3+1+22) ~=? length (validMoves queenBlockedByCloseFriend W)

queenBlockedByFarFriend = addPawn queenAllFree (5,7) W 
-- 3 Moves for King
-- 1 Move for Pawn
-- 24 Moves for Queen
testQueenBlockedByFarFriend = (3+1+24) ~=? length (validMoves queenBlockedByFarFriend W)

queenBlockedByCloseEnemy = addPawn queenAllFree (5,6) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 23 Moves for Queen
testQueenBlockedByCloseEnemy = (3+1+23) ~=? length (validMoves queenBlockedByCloseEnemy W)

queenBlockedByFarEnemy = addPawn queenAllFree (5,7) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 24 Moves for Queen
testQueenBlockedByFarEnemy = (3+1+24) ~=? length (validMoves queenBlockedByFarEnemy W)
-- Pawn is at the Edge, I can fully move
queenBlockedByFarerEnemy = addPawn queenAllFree (5,8) B 
-- 3 Moves for King
-- 26 Core Queen Moves
testQueenBlockedByFarerEnemy = (3+26) ~=? length (validMoves queenBlockedByFarerEnemy W)


queenBlockedDigByCloseEnemy = addPawn queenAllFree (6,6) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 23 Moves for Queen
testQueenBlockedDiagonalByCloseEnemy = (3+1+23) ~=? length (validMoves queenBlockedDigByCloseEnemy W)

queenBlockedDigByFarEnemy = addPawn queenAllFree (7,7) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 24 Moves for Queen
testQueenBlockedDiagonalByFarEnemy = (3+1+24) ~=? length (validMoves queenBlockedDigByFarEnemy W)

queenBlockedDigByCloseFriend = addPawn queenAllFree (6,6) W 
-- 3 Moves for King
-- 24 Moves for Queen
testQueenBlockedDiagonalByCloseFriend = (3+24) ~=? length (validMoves queenBlockedDigByCloseFriend W)

queenBlockedDigByFarFriend = addPawn queenAllFree (7,7) W 
-- 3 Moves for King 
-- 26 Moves for Queen
testQueenBlockedDiagonalByFarFriend = (3+26) ~=? length (validMoves queenBlockedDigByFarFriend W)


queenAtTheEdge = addQueen safeKings (5,8) W 
-- 3 Moves for King 
-- 21 Moves for Queen 
testQueenAtTheEdge = 24 ~=? length (validMoves queenAtTheEdge W)

-- =======================================
-- ========== Knight =====================
-- =======================================
allKnightMoves = TestList [
    TestLabel "All Free" testKnightAllFreeMoves
    ,TestLabel "Blocked by Friend" testKnightBlockedByFriend
    ,TestLabel "Blocked by Enemy" testKnightBlockedByEnemy
    ,TestLabel "At Edge of Board" testKnightAtTheEdge
    ,TestLabel "Two Fields are Blocked by Friends" testKnightBlockedByTwoFriends
    ,TestLabel "Two Fields are Blocked by Enemies" testKnightBlockedByTwoEnemies
    ,TestLabel "One Field blocked by Enemy, One by Friend" testKnightBlockedByFriendAndEnemy
 ]

knightAllFree = addKnight safeKings (5,5) W 
-- 3 Moves for King 
-- 8 Moves for Knight 
testKnightAllFreeMoves = (3+8) ~=? length (validMoves knightAllFree W)

knightBlockedByFriend = addPawn knightAllFree (6,3) W 
-- 3 Moves for King 
-- 1 Move for Pawn 
-- 7 Moves for Knight
testKnightBlockedByFriend = (3+1+7) ~=? length (validMoves knightBlockedByFriend W)

knightBlockedByEnemy = addPawn knightAllFree (6,3) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 7 Moves for Knight
testKnightBlockedByEnemy = (3+1+7) ~=? length (validMoves knightBlockedByEnemy W)

knightAtTheEdge = addKnight safeKings (5,8) W 
-- 3 Moves for King 
-- 4 Moves for Knight 
testKnightAtTheEdge = (3+4) ~=? length (validMoves knightAtTheEdge W)


knightBlockedByTwoFriends = addPawn knightBlockedByFriend (6,7) W 
-- 3 Moves for King 
-- 1 Move for Pawn in Field
-- 2 Moves for Pawn at Start 
-- 6 Moves for Knight
testKnightBlockedByTwoFriends = (3+1+2+6) ~=? length (validMoves knightBlockedByTwoFriends W)


knightBlockedByTwoEnemies = addPawn knightBlockedByEnemy (6,7) B 
-- 3 Moves for King 
-- 2 Move onto Pawn 
-- 6 Moves for Knight
testKnightBlockedByTwoEnemies = (3+2+6) ~=? length (validMoves knightBlockedByTwoEnemies W)


knightBlockedByFriendAndEnemy = addPawn knightBlockedByFriend (6,7) B 
-- 3 Moves for King 
-- 1 Move onto Pawn
-- 1 Move for friendly Pawn
-- 6 Other Moves for Knight
testKnightBlockedByFriendAndEnemy = (3+1+1+6) ~=? length (validMoves knightBlockedByFriendAndEnemy W)

-- =======================================
-- ========== Bishop =====================
-- =======================================
allBishopMoves = TestList [
    TestLabel "All Free" testBishopAllFree
    ,TestLabel "Blocked by Close Friend" testBishopBlockedByCloseFriend
    ,TestLabel "Blocked by Far Friend" testBishopBlockedByFarFriend
    ,TestLabel "Blocked by Close Enemy" testBishopBlockedByCloseEnemy
    ,TestLabel "Blocked by Far Enemy" testBishopBlockedByFarEnemy
    ,TestLabel "At Edge of Board" testBishopAtTheEdge
 ]

bishopAllFree = addBishop safeKings (5,5) W 
-- 3 Moves for King 
-- 12 Moves for Bishop 
testBishopAllFree = (3+12) ~=? length (validMoves bishopAllFree W)

bishopBlockedByCloseFriend = addPawn bishopAllFree (5,6) W 
-- 3 Moves for King 
-- 1 Move for Pawn 
-- 11 Moves for Bishop 
testBishopBlockedByCloseFriend = (3+1+11) ~=? length (validMoves bishopBlockedByCloseFriend W)


bishopBlockedByFarFriend = addPawn bishopAllFree (5,7) W 
-- 3 Moves for King 
-- 1 Move for Pawn 
-- 12 Moves for Bishop
testBishopBlockedByFarFriend = (3+1+12) ~=? length (validMoves bishopBlockedByFarFriend W)


bishopBlockedByCloseEnemy = addPawn bishopAllFree (5,6) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 11 Moves for Bishop
testBishopBlockedByCloseEnemy = (3+1+11) ~=? length (validMoves bishopBlockedByCloseEnemy W)

bishopBlockedByFarEnemy = addPawn bishopAllFree (6,7) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- 11 Moves for Bishop
testBishopBlockedByFarEnemy = (3+1+11) ~=? length (validMoves bishopBlockedByFarEnemy W)

bishopAtTheEdge = addBishop safeKings (5,8) W 
-- 3 Moves for King 
-- 7 Moves for Bishop 
testBishopAtTheEdge = (3+7) ~=? length (validMoves bishopAtTheEdge W)
        
-- =======================================
-- ========== Tower ======================
-- =======================================
allTowerMoves = TestList [
    TestLabel "All Free" testTowerAllFree
    ,TestLabel "Blocked by Close Friend" testTowerBlockedByCloseFriend
    ,TestLabel "Blocked by Far Friend" testTowerBlockedByFarFriend
    ,TestLabel "Blocked by Close Enemy" testTowerBlockedByCloseEnemy
    ,TestLabel "Blocked by Far Enemy" testTowerBlockedByFarEnemy
    ,TestLabel "Blocked by Far Enemy Other Direction" testTowerBlockedByFarEnemyOnOtherSide
    ,TestLabel "Blocked by Far Far Enemy" testTowerBlockedByFarerEnemy
    ,TestLabel "At Edge of Board" testTowerAtTheEdge
 ]

towerAllFree = addTower safeKings (5,5) W 
-- 3 Moves for King
-- A tower can move the whole board of 8 pieces with nothing in the way, but not his own field
-- Same for Y 
-- Resulting in (2x7) Moves
-- X Moves for Tower 
testTowerAllFree = (3+7+7) ~=? length (validMoves towerAllFree W)

towerBlockedByCloseFriend = addPawn towerAllFree (5,6) W 
-- 3 Moves for King 
-- 1 Move for Pawn 
-- All Tower Moves minus the 4 fields blocked by pawn 
testTowerBlockedByCloseFriend = (3+1+14-4) ~=? length (validMoves towerBlockedByCloseFriend W)

towerBlockedByFarFriend = addPawn towerAllFree (5,7) W 
-- 3 Moves for King 
-- 1 Move for Pawn 
-- All Tower Moves minus the 3 fields blocked by pawn 
testTowerBlockedByFarFriend = (3+1+15-3) ~=? length (validMoves towerBlockedByFarFriend W)


towerBlockedByCloseEnemy = addPawn towerAllFree (5,6) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- All Tower Moves minus the 3 fields blocked by pawn 
testTowerBlockedByCloseEnemy = (3+1+14-3) ~=? length (validMoves towerBlockedByCloseEnemy W)

towerBlockedByFarEnemy = addPawn towerAllFree (5,7) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- All Tower Moves minus the 2 fields blocked by pawn 
testTowerBlockedByFarEnemy = (3+1+14-2) ~=? length (validMoves towerBlockedByFarEnemy W)

towerBlockedByFarEnemyOtherSide = addPawn towerAllFree (5,3) B 
-- 3 Moves for King 
-- 1 Move onto Pawn 
-- All Tower Moves minus the 2 fields blocked by pawn 
testTowerBlockedByFarEnemyOnOtherSide = (3+1+14-3) ~=? length (validMoves towerBlockedByFarEnemyOtherSide W)

-- Enemy is on the edge, i can move normal
towerBlockedByFarerEnemy = addPawn towerAllFree (5,8) B 
-- 3 Moves for King
-- 14 base moves for Tower
testTowerBlockedByFarerEnemy = (3+14) ~=? length (validMoves towerBlockedByFarerEnemy W)

towerAtTheEdge = addTower safeKings (5,8) W 
-- 3 Moves for King 
-- 14 Moves for Tower  
testTowerAtTheEdge = (3+7+7) ~=? length (validMoves towerAtTheEdge W)
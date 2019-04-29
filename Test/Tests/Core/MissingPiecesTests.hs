module Tests.Core.MissingPiecesTests (allMissingPiecesTests) where

import Tests.TestSuite

allMissingPiecesTests = TestList [
    TestLabel "Empty Board White" testEmptyBoardWhiteMissingPieces
    ,TestLabel "Empty Board Black" testEmptyBoardBlackMissingPieces
    ,TestLabel "Full Board White" testFullBoardWhiteMissingPieces
    ,TestLabel "Full Board Black" testFullBoardBlackMissingPieces
    ,TestLabel "Only Queen + King White" testQueenAndKingWhiteMissingPieces
    ,TestLabel "Only Queen + King Black" testQueenAndKingBlackMissingPieces
    ,TestLabel "One Missing Tower" testOneMissingTowerMissingPieces
    ,TestLabel "Two Missing Towers" testTwoMissingTowerMissingPieces
    ]
-- There are 6 distinct figures on board
testEmptyBoardWhiteMissingPieces = 6 ~=? length ( missingPieces [] W)
testEmptyBoardBlackMissingPieces = 6 ~=? length ( missingPieces [] B)

testFullBoardWhiteMissingPieces = 0 ~=? length ( missingPieces initialBoard W )
testFullBoardBlackMissingPieces = 0 ~=? length ( missingPieces initialBoard B )

oneMissingTower = removeFigure initialBoard (1,8)
twoMissingTowers = removeFigure oneMissingTower (8,8)

-- There should be no difference if there is one or two towers missing
testOneMissingTowerMissingPieces =  1 ~=? length ( missingPieces oneMissingTower  W )
testTwoMissingTowerMissingPieces =  1 ~=? length ( missingPieces twoMissingTowers W )

testQueenAndKingWhiteMissingPieces = 4 ~=? length ( missingPieces (addQueen safeKings (5,5) W) W )
testQueenAndKingBlackMissingPieces = 4 ~=? length ( missingPieces (addQueen safeKings (5,5) B) B )
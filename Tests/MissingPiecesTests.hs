module Tests.MissingPiecesTests (allMissingPiecesTests) where

import Tests.TestSuite

allMissingPiecesTests = TestList [
    TestLabel "Empty Board White" testEBW
    ,TestLabel "Empty Board Black" testEBB
    ,TestLabel "Full Board White" testFBW
    ,TestLabel "Full Board Black" testFBB
    ,TestLabel "Only Queen + King White" testOQKW
    ,TestLabel "Only Queen + King Black" testOQKB
    ,TestLabel "One Missing Tower" testOMT
    ,TestLabel "Two Missing Towers" testTMT
    ]
-- There are 6 distinct figures on board
testEBW = 6 ~=? length ( missingPieces [] W)
testEBB = 6 ~=? length ( missingPieces [] B)

testFBW = 0 ~=? length ( missingPieces initialBoard W )
testFBB = 0 ~=? length ( missingPieces initialBoard B )

oneMissingTower = removeFigure initialBoard (1,8)
twoMissingTowers = removeFigure oneMissingTower (8,8)

-- There should be no difference if there is one or two towers missing
testOMT =  1 ~=? length ( missingPieces oneMissingTower  W )
testTMT =  1 ~=? length ( missingPieces twoMissingTowers W )

testOQKW = 4 ~=? length ( missingPieces (addQueen safeKings (5,5) W) W )
testOQKB = 4 ~=? length ( missingPieces (addQueen safeKings (5,5) B) B )
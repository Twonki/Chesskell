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

oneMissingTower = filter (\x-> pos x /= (1,8)) initialBoard
twoMissingTowers = filter (\x-> pos x /= (8,8)) oneMissingTower 

-- There should be no difference if there is one or two towers missing
testOMT =  1 ~=? length ( missingPieces oneMissingTower  W )
testTMT =  1 ~=? length ( missingPieces twoMissingTowers W )

addQueen :: Board -> Player -> Board
addQueen b c =(Chesspiece Queen (5,5) c):b

testOQKW = 4 ~=? length ( missingPieces (addQueen safeKings W) W )
testOQKB = 4 ~=? length ( missingPieces (addQueen safeKings B) B )
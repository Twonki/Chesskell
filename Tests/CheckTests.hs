module Tests.CheckTests (checkTests,checkMateTests) where

import Tests.TestSuite

checkTests = TestList [
    TestLabel "No Check White" testNCW
    ,TestLabel "No Check Black" testNCB
    ,TestLabel "FarAway Check" testFAC
    ,TestLabel "Close Check" testCC
    ,TestLabel "Check Double-Aggressor" testCDA
    ]

-- Reminder: Safekings means WhiteKing on 1/1 and BlackKing on 8/8

testNCW = False ~=? check safeKings W
testNCB = False ~=? check safeKings B

testFAC = True  ~=? check (addTower safeKings (7,1) B) W
testCC = True  ~=? check (addTower safeKings (2,1) B) W

cda = addTower (addTower safeKings (2,1) B) (1,7) B
testCDA = True  ~=? check cda W

checkMateTests = TestList [
    TestLabel "No Checkmate White" testNCMW
    ,TestLabel "No Checkmate Black" testNCMB
    ,TestLabel "True Checkmate White" testTCMW
    ,TestLabel "True Checkmate Black" testTCMB
    ,TestLabel "King can move out of Check" testKMC
    ,TestLabel "Kill Aggressor" testKA
    ,TestLabel "ScapeGoat a Piece" testSGP   
    ]

testNCMW = False ~=? checkmate safeKings W
testNCMB = False ~=? checkmate safeKings B

cmw = addTower (addTower safeKings (1,5) B) (2,5) B
testTCMW = True ~=? checkmate cmw W

cmb = addTower (addTower safeKings (7,5) W) (8,5) W
testTCMB = True ~=? checkmate cmb B

-- King can simply move out of check
testKMC = False ~=? checkmate (addTower safeKings (8,5) W) B 

-- Another Unit can kill the Agressor, resolving check
-- A Black tower can kill the White tower producing check
ka= addTower cmb (8,4) B
testKA = False ~=? checkmate ka B

-- take cmb from above
-- A tower can move in to safe the king 
sgp = addTower cmb (2,7) B
testSGP = False ~=? checkmate sgp B
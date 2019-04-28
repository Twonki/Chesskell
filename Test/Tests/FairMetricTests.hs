module Tests.FairMetricTests (allFairMetricTests) where

import Tests.TestSuite

allFairMetricTests = TestList [
    TestLabel "Fair Simple" testFS
    ,TestLabel "Fair RatedSimple" testFRS
    ,TestLabel "Fair Agility" testFA
    ]

testFS = True ~=? (simple initialBoard W) == (simple initialBoard B)
testFRS = True ~=? (ratedSimple initialBoard W) == (ratedSimple initialBoard B)
testFA = True ~=? (agility initialBoard W) == (agility initialBoard B)
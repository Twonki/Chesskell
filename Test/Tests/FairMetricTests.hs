module Tests.FairMetricTests (allFairMetricTests) where

import Tests.TestSuite

allFairMetricTests = TestList [
    TestLabel "Fair Simple" testFairSimpleMetric
    ,TestLabel "Fair RatedSimple" testFairRatedSimpleMetric
    ,TestLabel "Fair Agility" testFairAgilityMetric
    ]

testFairSimpleMetric = True ~=? (simple initialBoard W) == (simple initialBoard B)
testFairRatedSimpleMetric = True ~=? (ratedSimple initialBoard W) == (ratedSimple initialBoard B)
testFairAgilityMetric = True ~=? (agility initialBoard W) == (agility initialBoard B)
module AllTests where

import Test.HUnit

import Tests.PawnTests
import Tests.InitialBoardTests
import Tests.FairMetricTests

allTests = TestList [
   TestLabel "PawnTests" allPawnTests
   ,TestLabel "BoardTests" allBoardTests
   ,TestLabel "FairMetricTests" allFairMetricTests
   ]

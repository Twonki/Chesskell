module AllTests where

import Test.HUnit

import Tests.PawnTests
import Tests.InitialBoardTests

allTests = TestList [
   TestLabel "PawnTests" allPawnTests
   ,TestLabel "BoardTests" allBoardTests
   ]

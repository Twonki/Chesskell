module AllTests where

import Test.HUnit

import Tests.PawnTests

allTests = TestList [
   TestLabel "PawnTests" allPawnTests
   ]

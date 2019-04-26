module AllTests where

import Test.HUnit

import Tests.PawnTests
import Tests.InitialBoardTests
import Tests.FairMetricTests
import Tests.MissingPiecesTests
import Tests.CheckTests
import Tests.PawnReplacementTests
import Tests.MovementTests

import Tests.TestSuite

allTests = TestList [
   TestLabel "PawnTests" allPawnTests
   ,TestLabel "BoardTests" allBoardTests
   ,TestLabel "FairMetricTests" allFairMetricTests
   ,TestLabel "MissingPiecesTests" allMissingPiecesTests
   ,TestLabel "CheckTests" checkTests
   ,TestLabel "CheckmateTests" checkMateTests
   ,TestLabel "CheckLimitedMovesTests" checkLimitedMoveTests
   ,TestLabel "Pawn Replacement" allPawnReplacementTests
   ,TestLabel "Movement Tests" allMovementTests
   ]

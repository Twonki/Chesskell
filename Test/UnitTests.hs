module Main where

import Test.HUnit

import Test.Framework
import Test.Framework.Providers.HUnit

import Tests.PawnTests
import Tests.InitialBoardTests
import Tests.FairMetricTests
import Tests.MissingPiecesTests
import Tests.CheckTests
import Tests.PawnReplacementTests
import Tests.MovementTests
import Tests.CoreMovementTests

allTests = TestList [
   TestLabel "PawnTests" allPawnTests
   ,TestLabel "BoardTests" allBoardTests
   ,TestLabel "FairMetricTests" allFairMetricTests
   ,TestLabel "MissingPiecesTests" allMissingPiecesTests
   ,TestLabel "CheckTests" checkTests
   ,TestLabel "CheckmateTests" checkMateTests
   ,TestLabel "CheckLimitedMovesTests" checkLimitedMoveTests
   ,TestLabel "Pawn Replacement" allPawnReplacementTests
   ,TestLabel "Core Movement Tests" allCoreMovementTests
   ,TestLabel "Movement Tests" allMovementTests
   ]

tests = hUnitTestToTests allTests
main = defaultMain tests
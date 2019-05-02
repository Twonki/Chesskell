module Main where

import Test.HUnit

import Test.Framework
import Test.Framework.Providers.HUnit

import Tests.Movement.PawnTests
import Tests.Game.InitialBoardTests
import Tests.Game.FairMetricTests
import Tests.Core.MissingPiecesTests
import Tests.Game.CheckTests
import Tests.Game.PawnReplacementTests
import Tests.Game.GameTests
import Tests.Movement.MovementTests
import Tests.Core.CoreMovementTests
import Tests.Core.MoveToTests

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
   ,TestLabel "Move To Tests" allMoveToTests
   ,TestLabel "GameState Tests" allGameTests
   ]

tests = hUnitTestToTests allTests
main = defaultMain tests
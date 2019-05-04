module Main where

import Test.HUnit

import Test.Framework
import Test.Framework.Providers.HUnit

import Tests.PawnTests
import Tests.CheckTests
import Tests.GameTests
import Tests.MovementTests
import Tests.CoreMovementTests

allTests = TestList [
   TestLabel "PawnTests" allPawnTests
   ,TestLabel "Check-Mechanic-Tests" allCheckTests
   ,TestLabel "Core Movement Tests" allCoreMovementTests
   ,TestLabel "Movement Tests" allMovementTests
   ,TestLabel "GameState Tests" allGameTests
   ]

tests = hUnitTestToTests allTests
main = defaultMain tests
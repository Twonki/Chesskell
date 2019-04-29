module Tests.Core.MoveToTests (allMoveToTests) where

import Data.Maybe(isNothing)
import Tests.TestSuite

allMoveToTests = TestList [
    TestLabel "MoveTo on Free Field is Just" testFreeField
    ,TestLabel "MoveTo on Ally Field is Nothing" testMoveOntoAllyField
    ,TestLabel "MoveTo on Enemy Field is Just " testMoveOntoEnemyField
    ,TestLabel "MoveTo on Enemy Field removes Enemy" testMoveOntoEnemyRemoval
    ,TestLabel "MoveTo ignores Normal Piece Movement-Restrictions" testMoveToNoGameLogic1
    ,TestLabel "MoveTo ignores normal Board-Size Restrictions" testMoveToNoGameLogic2
    ]
-- A simple tower to perform these tests
tower = Chesspiece Tower (5,5) W
-- A Board with the single tower is my basecase
base = tower:[]

-- I can move here, it`s free
testFreeField = False ~=? isNothing (moveTo base tower (5,6))

-- A friendly piece is in the way
alliedField = addPawn base (5,6) W
-- I cannot move here
testMoveOntoAllyField = True ~=? isNothing (moveTo alliedField tower (5,6))

-- A enemy piece is in the way
enemyField = addPawn base (5,6) B
-- I can move here
testMoveOntoEnemyField = False ~=? isNothing (moveTo enemyField tower (5,6))
-- If i do, the pawn gets removed
testMoveOntoEnemyRemoval = 1 ~=? length ( fromJust' (moveTo enemyField tower (5,6)))

-- I can move even if tower cannot move there
testMoveToNoGameLogic1 = False ~=? isNothing (moveTo base tower (6,6))

-- I can move outside the field
testMoveToNoGameLogic2 = False ~=? isNothing (moveTo base tower (10,22))


fromJust' :: Maybe Board -> Board 
fromJust' (Just b) = b 
fromJust' Nothing = []
    
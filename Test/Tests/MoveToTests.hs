module Tests.MoveToTests (allMoveToTests) where

import Data.Maybe(isNothing)
import Tests.TestSuite

allMoveToTests = TestList [
    TestLabel "Move on Free Field" testFF
    ,TestLabel "Move on Ally Field" testAF
    ,TestLabel "Move on Enemy Field" testEF
    ,TestLabel "Move on Enemy Field - Enemy removed" testER
    ,TestLabel "No Position-Restrictions in MoveTo I" testNoPR1
    ,TestLabel "No Position-Restrictions in MoveTo II" testNoPR2
    ]

tower = Chesspiece Tower (5,5) W
base = tower:[]

-- I can move here, it`s free
testFF = False ~=? isNothing (moveTo base tower (5,6))

-- A friendly piece is in the way
af = addPawn base (5,6) W
-- I cannot move here
testAF = True ~=? isNothing (moveTo af tower (5,6))

-- A enemy piece is in the way
ef = addPawn base (5,6) B
-- I can move here
testEF = False ~=? isNothing (moveTo ef tower (5,6))
-- If i do, the pawn gets removed
testER = 1 ~=? length ( fromJust' (moveTo ef tower (5,6)))

-- I can move even if tower cannot move there
testNoPR1 = False ~=? isNothing (moveTo base tower (6,6))

-- I can move outside the field
testNoPR2 = False ~=? isNothing (moveTo base tower (10,22))


fromJust' :: Maybe Board -> Board 
fromJust' (Just b) = b 
fromJust' Nothing = []
    
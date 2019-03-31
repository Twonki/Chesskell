module Tests.TestSuite (
    module Chess.Movement,
    module Chess.CoreMovement,
    module Chess.Figures,
    module Test.HUnit,
    safeKings,
    countMoves
)where
import Chess.Movement
import Chess.Figures
import Chess.CoreMovement
import Test.HUnit

-- Movement does not work if the kings are endangered! 
-- This simple function puts 2 kings in the end of the boards, so i can slim down the rest
safeKings :: Board
safeKings = [ChessFigure King (1,1) W, ChessFigure King (8,8) B]

countMoves :: [Maybe Board] -> Int 
countMoves = length . clearMaybeBoard
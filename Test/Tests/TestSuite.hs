module Tests.TestSuite (
    module Chess.Movement,
    module Chess.CoreMovement,
    module Chess.Figures,
    module Chess.Metrics,
    module Chess.Game,
    module Test.HUnit,
    safeKings,
    countMoves,
    addPawn,
    addQueen,
    addKing,
    addBishop,
    addKnight,
    addTower,
    addPiece,
    fromJust',
    removeFigure
)where
import Chess.Movement
import Chess.Figures
import Chess.CoreMovement
import Chess.Metrics
import Chess.Game
import Data.Maybe(catMaybes)
import Test.HUnit

-- Movement does not work if the kings are endangered! 
-- This simple function puts 2 kings in the end of the boards, so i can slim down the rest
safeKings :: Board
safeKings = [Chesspiece King (1,1) W, Chesspiece King (8,8) B]

countMoves :: [Maybe Board] -> Int 
countMoves = length . catMaybes

addPiece :: Board -> Chesspiece -> Board 
addPiece b p = p : b

buildPiece :: Figure -> Pos -> Player -> Chesspiece
buildPiece = Chesspiece

addPawn :: Board -> Pos -> Player -> Board 
addPawn b p c = addPiece b (Chesspiece Pawn p c)

addKing :: Board -> Pos -> Player -> Board 
addKing b p c = addPiece b (Chesspiece King p c)

addKnight :: Board -> Pos -> Player -> Board 
addKnight b p c = addPiece b (Chesspiece Knight p c)

addBishop :: Board -> Pos -> Player -> Board 
addBishop b p c = addPiece b (Chesspiece Bishop p c)

addTower :: Board -> Pos -> Player -> Board 
addTower b p c = addPiece b (Chesspiece Tower p c)

addQueen :: Board -> Pos -> Player -> Board 
addQueen b p c = addPiece b (Chesspiece Queen p c)

removeFigure :: Board -> Pos -> Board 
removeFigure b p = filter (\cp -> pos cp /= p) b


fromJust' :: Maybe Board -> Board 
fromJust' (Just b) = b 
fromJust' Nothing = []

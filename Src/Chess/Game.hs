module Chess.Game (
    GameState,
    Move, 
    changePlayer',
    movePiece,
    canPickUp,
    lost,
    showBoard,
    initialGameState
)

where 

import Chess.Movement
import Chess.Metrics
import Chess.Figures

type GameState = (Board, Player)
type Move = (Pos,Pos)

changePlayer' :: GameState -> GameState
changePlayer' (b,p) = (b, changePlayer p)

movePiece :: GameState -> Move ->  GameState
movePiece g@(b,c) (s,e) =
    let f = pieceOnPos b s
    in
        if canPickUp s g
        then case f of
                (Just f') ->
                        let 
                            vs = validMoves b c 
                            m = moveTo b f' e
                        in case m of 
                            Just m' -> 
                                if m' `elem` vs 
                                then (m',changePlayer c)
                                else g 
                            Nothing -> g
                Nothing -> g 
        else g -- Someone Tried to do something invalid - nothing happens

-- Just Tells whether a player can pickup the piece on a given position
-- This is about to catch if someone wants to pickup an empty field or an enemy piece
canPickUp:: Pos -> GameState -> Bool 
canPickUp p (b,c) = 
    case f of 
        Nothing -> False 
        Just f' -> c == player f'
    where f = pieceOnPos b p

lost :: GameState -> Player -> Bool 
lost g@(b,c) = checkmate b

initialGameState = (initialBoard,W)

showBoard = printBoard
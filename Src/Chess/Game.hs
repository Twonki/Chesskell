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
changePlayer' = (\(b,p)->(b, changePlayer p))

movePiece :: GameState -> Move ->  GameState
movePiece g@(b,c) (s,e) = 
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
                            otherwise -> g
                otherwise -> g 
        else g -- Someone Tried to do something invalid - nothing happens
    where f = pieceOnPos b s

canPickUp:: Pos -> GameState -> Bool 
canPickUp p (b,c) = 
    case f of 
        Nothing -> False 
        Just f' -> c == (player f')
    where f = pieceOnPos b p

lost :: GameState -> Player -> Bool 
lost g@(b,c) c' = checkmate b c'

initialGameState = (initialBoard,W)

showBoard = printBoard
module Game where

import Chess.Movement
import Chess.Metrics

type GameState = (Board, Player)
type Move = (Pos,Pos)

main :: IO () 
main = do
    let g@(b,c) = initialGameState
    putStrLn "Hello, lets start Chess!"
    

changePlayer' :: GameState -> GameState
changePlayer' = (\(b,p)->(b, changePlayer p))

movePiece :: GameState -> Move ->  GameState
movePiece g@(b,c) (s,e) = 
        if canPickUp s g
        then case f of
                (Just f') ->
                        let  m = moveTo b f' e
                        in case m of 
                            Just m' -> (m',changePlayer c) 
                            otherwise -> g
                otherwise -> g 
        else g -- Someone Tried to do something invalid - nothing happens
    where f = pieceOnPos b s

canPickUp:: Pos -> GameState -> Bool 
canPickUp p (b,c) = demaybiebool $ fmap (((==) c) . player) f
    where f = pieceOnPos b p

-- Helpers 
lost :: GameState -> Player -> Bool 
lost g@(b,c) c' = checkmate b c'
won g c = lost g (changePlayer c)

initialGameState = (initialBoard,W)

demaybiebool :: Maybe Bool -> Bool 
demaybiebool Nothing = False 
demaybiebool (Just x) = x
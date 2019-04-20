module Game where

import Chess.Movement
import Chess.Metrics

import Control.Monad.State.Lazy

type GameState = (Board, Player)
type Game = StateT GameState IO

type Move = (Pos,Pos)

changePlayer' :: GameState -> GameState
changePlayer' = (\(b,p)->(b, changePlayer p))

movePiece :: Pos -> Pos -> GameState -> GameState
movePiece s e g@(b,c) = 
        if canPickUp s g
        then 
            case f of
                (Just f') -> 
                    if canMove f' e g 
                    then 
                        let  m = moveTo b f' e
                        in 
                            case m of 
                                Just m' -> (m',changePlayer c) 
                                otherwise -> g
                    else g 
        else g -- Someone Tried to do something invalid - nothing happens
    where f = pieceOnPos b s

canPickUp:: Pos -> GameState -> Bool 
canPickUp p (b,c) = demaybiebool maybeSameColour
    where f = pieceOnPos b p
          maybeSameColour = fmap (((==) c) . player) f 

canMove:: Chesspiece -> Pos -> GameState -> Bool 
canMove f p g@(b,c)
        | trymove == Nothing = False 
        | otherwise = True 
    where trymove = moveTo b f p

demaybiebool :: Maybe Bool -> Bool 
demaybiebool Nothing = False 
demaybiebool (Just x) = x 
module Game where

import Chess.Movement
import Chess.Metrics

type GameState = (Board, Player)
type Move = (Pos,Pos)

main :: IO () 
main = do
    let g@(b,c) = initialGameState
    putStrLn "Hello, lets start Chess!"
    inp <- getLine 
    let s@(x,y) = read (inp) :: (Int,Int)
    putStrLn (show s)

gameLoop :: GameState -> IO GameState
gameLoop oldS@(b,c) = do 
    putStrLn ("Its " ++ show c ++ " turn")
    putStrLn "Which piece to move?"
    inp <- getLine 
    let s = read (inp) :: (Int,Int)
    putStrLn "Where to move?"
    inp2 <- getLine 
    let e = read (inp2) :: (Int,Int)
    putStrLn ("Move " ++ show s ++ " to " ++ show e)
    let newS = movePiece oldS (s,e)
    if (newS == oldS) 
        then 
            do
                putStrLn "Invalid move! Try Again"
                gameLoop oldS
        else 
            gameLoop newS
            

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
module Game where

import Chess.Movement
import Chess.Metrics

type GameState = (Board, Player)
type Move = (Pos,Pos)

main :: IO () 
main = do
    putStrLn "Hello, lets start Chess!"
    gameLoop initialGameState

gameLoop :: GameState -> IO ()
gameLoop oldS@(b,c) = do 
    if (lost oldS c)
    then do putStrLn ( show c ++ " lost")
    else do
        putStrLn (printBoard b)
        putStrLn ("Its " ++ show c ++ "s turn")
        move <- readMove
        let newS = movePiece oldS move
        if (newS == oldS) 
        then 
            do
                putStrLn "Invalid move! Try Again"
                gameLoop oldS
        else gameLoop newS
            
readMove :: IO Move
readMove = do 
    putStrLn "Which piece to move?"
    inp <- getLine 
    let s = read (inp) :: (Int,Int)
    putStrLn "Where to move?"
    inp2 <- getLine 
    let e = read (inp2) :: (Int,Int)
    putStrLn ("Move " ++ show s ++ " to " ++ show e)
    return (s,e)

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
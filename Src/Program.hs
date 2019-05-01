module Main where

import Chess.Game

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
            
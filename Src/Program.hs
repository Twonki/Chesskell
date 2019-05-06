module Main where

import Chess.Game

main :: IO () 
main = do
    let g@(b,c) = initialGameState
    putStrLn "Hello, lets start Chess!"
    gameLoop g

gameLoop :: GameState -> IO ()
gameLoop oldS@(b,c) =
    if lost oldS c 
    then putStrLn (show c ++ " lost!") 
    else do
        putStrLn ("Its " ++ show c ++ " turn")
        putStrLn (showBoard b)
        move <- askMove
        let newS = movePiece oldS move
        if newS == oldS 
            then 
                do
                    putStrLn "Invalid move! Try Again"
                    gameLoop oldS
            else gameLoop newS

askMove :: IO Move
askMove = do 
    putStrLn "Which piece to move?"
    inp <- getLine 
    let s = read inp :: (Int,Int)
    putStrLn "Where to move?"
    inp2 <- getLine 
    let e = read inp2 :: (Int,Int)
    putStrLn ("Move " ++ show s ++ " to " ++ show e)
    return (s,e)
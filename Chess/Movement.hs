module Chess.Movement (
    module Chess.Figures,
    validMoves,
    allMoves,
    moves,
    clearMaybeBoard,
    check,
    checkmate
)where

import  Data.List.Split (splitOn)
import  Chess.Figures
import  Chess.CoreMovement
import  Control.Monad (liftM)


moveTo :: Board -> Chesspiece -> Pos -> Maybe Board
moveTo b f p = 
        if attackable
        then Just $ (draw f p) : b''
        else Nothing --Invalid Move - cannot hit there!
    where   b' = filter (\n-> n /= f) b 
            other  = getFigOnPos b' p
            attackable = case other of 
                Nothing -> True --There is nothing, so i can move her
                Just m -> canAttack f m -- there is something, i have to check if i can attack
            b'' = case other of 
                Nothing -> b' -- Nothing killed
                Just other -> filter (\n-> n /= other) b' --I killed e
            
moves :: Board -> Chesspiece -> [Maybe Board]
moves b fig@(Chesspiece t p c) 
    | t == Pawn = validPawnMoves b fig 
    | otherwise = map (moveTo b fig) possibleMoves
    where b' = filter (\n-> n /= fig) b   
          routine = concat . map (moveFilter b') .  map ($p) 
          possibleMoves = 
            case t  of  
              Bishop -> routine [risingDigL,fallingDigR,risingDigR,fallingDigL]
              Tower ->  routine [ups,downs,lefts,rights]
              Queen ->  routine $ [ups,downs,lefts,rights] ++ [risingDigL,fallingDigR,risingDigR,fallingDigL]
              King ->   routine [kingMoves]
              Knight -> routine [knightMoves]

validPawnMoves :: Board -> Chesspiece -> [Maybe Board]
-- Input: current Board, Pawn 
-- Used to filter allPawnMoves 
-- Step 1: Get all Moves for Pawn
-- Step 2: If i reach back-end, new Boards with lost figures
-- for 2: check on y, get lost figures
-- Step 3: Filter for hittibility
-- 3.1 only move forward if nothing 
-- 3.2 only move diagonal if hittable (check moves on x for diagonality)
validPawnMoves b fig@(Chesspiece t p c) = undefined
    where 
        posMoves = pawnMoves p c
        attackMoves = [a |  a <- posMoves, fst a /= fst p]
        forwardMoves = [a |  a <- posMoves, fst a /= fst p]
        validAttacks = undefined -- Filter only attackables
        validForward = undefined -- Filter only free fields
        valids = validForward ++ validAttacks
        finishers 
            | c == W = [a |  a <- valids, snd a = 8]
            | c == B = [a |  a <- valids, snd a = 1]

-- stopAtNearest takes positions -> stoppers -> positions
-- For my use it needs to be flipped
moveFilter :: Board -> [Pos] -> [Pos]            
moveFilter b = (flip stopAtNearest) (takenPositions b) . filter onBoard

-- Filter every possible moves and only allowes the one where i´m not in check
-- Required to kill a deadlock i produced with allmoves and check
validMoves :: Board -> Player -> [Board]
validMoves b p = filter (flip check p) $ (allMoves b p)

-- Every reachable position, without check
allMoves :: Board -> Player -> [Board]
allMoves b p =  clearMaybeBoard (concat $ map (moves b) fs)
    where fs = getFigsForPlayer b p 

clearMaybeBoard :: [Maybe Board] -> [Board]
clearMaybeBoard [] = [] 
clearMaybeBoard (x:xs) = case x of 
        Just x -> x : clearMaybeBoard xs
        Nothing -> clearMaybeBoard xs

check :: Board -> Player -> Bool
check b p = foldr (||) False $ map (not . hasKing) myFigures
    where   
        enemyMoves = allMoves b $ changePlayer p 
        myFigures = map (flip getFigsForPlayer p) enemyMoves

checkmate :: Board -> Player -> Bool
checkmate b p = [] == allMoves b p
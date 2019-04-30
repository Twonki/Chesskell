module Chess.Movement (
    module Chess.Figures,
    validMoves,
    allMoves,
    moves,
    moveTo,
    clearMaybeBoard,
    check,
    checkmate
)where

import  Data.List.Split (splitOn)
import  Chess.Figures
import  Chess.CoreMovement
import Control.Monad((>>=), join)

moveTo :: Board -> Chesspiece -> Pos -> Maybe Board
moveTo b f p = 
        if attackable
        then Just $ (draw f p) : b''
        else Nothing --Invalid Move - cannot hit there!
    where   b' = removePiece b f
            other  = pieceOnPos b' p
            attackable = case other of 
                Nothing -> True --There is nothing, so i can move her
                Just m -> canAttack f m -- there is something, i have to check if i can attack
            b'' = case other of 
                Nothing -> b' -- Nothing killed
                Just other -> removePiece b' other --I killed e
            
moves :: Board -> Chesspiece -> [Maybe Board]
moves b fig@(Chesspiece t p c) 
    | t == Pawn = validPawnMoves b fig 
    | otherwise = moveTo b fig <$> (moveFilter b') (possibleMoves fig)
    where  b' = removePiece b fig

validPawnMoves :: Board -> Chesspiece -> [Maybe Board]
-- Input: current Board, Pawn 
-- Used to filter allPawnMoves 
-- Step 1: Get all Moves for Pawn
-- Step 2: If i reach back-end, new Boards with lost figures
-- for 2: check on y, get lost figures
-- Step 3: Filter for hittibility
-- 3.1 only move forward if nothing 
-- 3.2 only move diagonal if hittable (check moves on x for diagonality)
validPawnMoves b fig@(Chesspiece t p c)
        | length finishers > 0 = --Replace Logic
            let replacingPawns = (\t -> draw fig t) <$> finishers
            in  concat $ replacePawn' b <$> replacingPawns
        | otherwise =  moveTo b fig <$> valids
    where 
        posMoves = pawnMoves p c
        attackMoves = [a |  a <- posMoves, fst a /= fst p]
        forwardMoves = [a |  a <- posMoves, fst a == fst p]
        attackFigs = demaybefy [pieceOnPos b a | a <- attackMoves, not (free a b)]
        attackFigs' = filter (\y -> canAttack y fig) attackFigs
        validAttacks = [pos c | c <-  attackFigs'] -- Filter only attackables
        validForward = [c | c <- forwardMoves , free c b] -- Filter only free fields
        valids = validForward ++ validAttacks
        finishers 
            | c == W = [a |  a <- valids, snd a == 1]
            | c == B = [a |  a <- valids, snd a == 8]

-- Filter every possible moves and only allowes the one where i´m not in check
-- Required to kill a deadlock i produced with allmoves and check
validMoves :: Board -> Player -> [Board]
validMoves b p = filter (\l -> not (check l p)) $ (allMoves b p)

-- Every reachable position, without check
allMoves :: Board -> Player -> [Board]
allMoves b p =  clearMaybeBoard (concat $ moves b <$> fs)
    where fs = piecesForPlayer b p 

clearMaybeBoard :: [Maybe Board] -> [Board]
clearMaybeBoard = demaybefy

check :: Board -> Player -> Bool
check b p = foldr (||) False $ not . hasKing <$> myFigures
    where   
        enemyMoves = allMoves b $ changePlayer p 
        myFigures = flip piecesForPlayer p <$> enemyMoves

checkmate :: Board -> Player -> Bool
checkmate b p = [] == validMoves b p

demaybefy :: [Maybe a] -> [a]
demaybefy [] = []
demaybefy (x:xs) = case x of 
    Just x -> x : demaybefy xs
    Nothing -> demaybefy xs

replacePawn :: Board -> Chesspiece -> [Board]
replacePawn b piece@(Chesspiece Pawn p c) = 
    let b' = removePiece b piece
        missing = missingPieces b' c
        replacers = (\mf -> Chesspiece{typ=mf,pos=p,player=c}) <$> missing
    in  [r:b' | r <- replacers] 

replacePawn' :: Board -> Chesspiece -> [Maybe Board]
replacePawn' b p= Just <$> (flip removePiece) p <$> replacePawn b p
module Chess.Movement (
    module Chess.Figures,
    validMoves,
    allMoves,
    moves,
    moveTo,
    check,
    checkmate,
    replacePawn
)where

import Chess.Figures
import Chess.CoreMovement
import Control.Monad((>>=), join)
import Data.Maybe(catMaybes)
import Control.Arrow((&&&))

moveTo :: Board -> Chesspiece -> Pos -> Maybe Board
moveTo b f p = 
        if attackable
        then Just $ draw f p : b''
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
moves b fig@(Chesspiece t _ _) 
    | t == Pawn = validPawnMoves b fig 
    | otherwise =
        let validMoves = join $ moveFilter b' <$> possibleMoves fig
        in moveTo b fig <$> validMoves
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
validPawnMoves b fig@(Chesspiece Pawn p c)
        | not (null replacementPositions) = --Replace Logic
            let 
                replaceTuples =  (\p ->(draw fig p,moveTo b fig p)) <$> replacementPositions
                replaceTuples' = [(a,b) | (a,Just b) <- replaceTuples]
                doReplace (replacer,replacementBoard) = replacePawn replacementBoard replacer
                replacementMoves = doReplace  =<< replaceTuples'
            in  Just <$> replacementMoves
        | otherwise =  moveTo b fig <$> valids
    where 
        posMoves = pawnMoves p c
        attackMoves = [a |  a <- posMoves, fst a /= fst p]
        forwardMoves = [a |  a <- posMoves, fst a == fst p]
        attackFigs = filter (canAttack fig) $ catMaybes [pieceOnPos b a | a <- attackMoves, not (free a b)]
        valids = [pos piece | piece <-  attackFigs] ++ [position | position <- forwardMoves , free position b]
        replacementPositions 
            | c == W = [a |  a <- valids, snd a == 1]
            | c == B = [a |  a <- valids, snd a == 8]

-- Filter every possible moves and only allowes the one where i´m not in check
validMoves :: Board -> Player -> [Board]
validMoves b p = filter (\l -> not (check l p)) $ allMoves b p

-- Every reachable position, without check
allMoves :: Board -> Player -> [Board]
allMoves b p =  catMaybes $ fs >>= moves b 
    where fs = piecesForPlayer b p 

check :: Board -> Player -> Bool
check b p = or $ not . hasKing <$> myFigures
    where   
        enemyMoves = allMoves b $ changePlayer p 
        myFigures = flip piecesForPlayer p <$> enemyMoves

checkmate :: Board -> Player -> Bool
checkmate b p = null $ validMoves b p

replacePawn :: Board -> Chesspiece -> [Board]
replacePawn b piece@(Chesspiece Pawn p c) = 
    let b' = removePiece b piece
        missing = missingPieces b' c
        replacers = (\mf -> Chesspiece{typ=mf,pos=p,player=c}) <$> missing
    in  [r:b' | r <- replacers] 
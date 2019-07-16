module Chess.Movement (
    module Chess.Figures,
    validMoves,
    allMoves,
    moves,
    moveTo,
    check,
    checkmate,
    replacePawn
)

where

import Chess.Figures
import Chess.CoreMovement
import Control.Monad((>>=), join)
import Data.Maybe(catMaybes)

moveTo :: Board -> Chesspiece -> Pos -> Maybe Board
moveTo b f p = 
        if attackable
        then Just $ draw f p : b''
        else Nothing --Invalid Move - cannot hit there!
    where   b' = removePiece b f
            other  = pieceOnPos b' p
            attackable = case other of 
                Nothing -> True         -- There is nothing, so i can move her
                Just m -> canAttack f m -- There is something, i have to check if i can attack
            b'' = case other of 
                Nothing -> b' -- Nothing killed
                Just other -> removePiece b' other --I killed e
            
-- Takes the board and the chesspiece to move, and returns the possible boards
moves :: Board -> Chesspiece -> [Maybe Board]
moves b fig@(Chesspiece t _ _) 
    | t == Pawn = validPawnMoves b fig -- Because Pawns are very special, they have their own movement done below. IMPORTANT: We pass b, not b' 
    | otherwise =
        let validMoves = moveFilter b' =<< possibleMoves fig 
        in moveTo b fig <$> validMoves
    where  b' = removePiece b fig

{- 
Input: current Board, Pawn 
Used to filter allPawnMoves 
Step 1: Get all Moves for Pawn
Step 2: If i reach back-end, new Boards with lost figures
for 2: check on y, get lost figures
Step 3: Filter for hittibility
3.1 only move forward if nothing 
3.2 only move diagonal if hittable (check moves on x for diagonality)
-}
validPawnMoves :: Board -> Chesspiece -> [Maybe Board]
validPawnMoves b fig@(Chesspiece Pawn p c)
        | not (null replacementPositions) = --Replace Logic
            let 
                replaceTuples =  (\p ->(draw fig p,moveTo b fig p)) <$> replacementPositions -- Check for every possible picked up figure where it can move
                replaceTuples' = [(a,b) | (a,Just b) <- replaceTuples] -- Patternmatch on only the possible replacement tupels
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
        replacementPositions -- Replacement Positions = Those Pawns which may "finish" and become any possible figure
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
check b p = or $ not . hasKing <$> myFigures -- I in Check if the enemy can to a move which removes my king
    where   
        enemyMoves = allMoves b $ changePlayer p 
        myFigures = flip piecesForPlayer p <$> enemyMoves

checkmate :: Board -> Player -> Bool
checkmate b p = null $ validMoves b p -- I lost when i have not any move left (After checking for check)

replacePawn :: Board -> Chesspiece -> [Board]
replacePawn b piece@(Chesspiece Pawn p c) = 
    let b' = removePiece b piece -- Dont count the pawn for the next board
        missing = missingPieces b' c -- Look for missing pieces in the next board for MY player (the one which is moving the pawn)
        replacers = (\mf -> Chesspiece{typ=mf,pos=p,player=c}) <$> missing -- Check which figures are missing for MY player (the colour moving the pawn) and make a new possible figure for every missing piece on the position the pawn moved 
    in  [r:b' | r <- replacers] -- Return the (field without my pawn)+the replacer for every possible replacer
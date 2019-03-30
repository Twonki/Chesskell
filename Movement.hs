module Movement (
    module Figures,
    allMoves,
    moves
)where

import  Data.List.Split (splitOn)
import  Figures
import  CoreMovement

-- A Chessfigure can move "onto" another Chessfigure if it has a different colour
canAttack :: ChessFigure -> ChessFigure -> Bool
canAttack a b = player a /= player b

moveTo :: Board -> ChessFigure -> Pos -> Maybe Board
moveTo b f p = 
        if attackable
        then Just $ (draw f p) : b''
        else Nothing --Invalid Move - cannot hit there!
    where   b' = filter (\n-> n /= f) b 
            e  = getFigOnPos b' p
            attackable = case e of 
                Nothing -> True 
                Just m -> canAttack f m
            b'' = case e of 
                Nothing -> b' 
                Just e -> filter (\n-> n /= e) b'
            
moves :: Board -> ChessFigure -> [Maybe Board]
moves b fig@(ChessFigure t p c) = map (moveTo b fig) possibleMoves
    where b' = filter (\n-> n /= fig) b   
          routine = concat . map (moveFilter b') .  map ($p) 
          possibleMoves = 
            case t  of  
              Bishop -> routine [risingDigL,fallingDigR,risingDigR,fallingDigL]
              Tower ->  routine [ups,downs,lefts,rights]
              Queen ->  routine $ [ups,downs,lefts,rights] ++ [risingDigL,fallingDigR,risingDigR,fallingDigL]
              King ->   routine [kingMoves]
              Knight -> routine [knightMoves]
              otherwise -> moveFilter b $ pawnMoves p c

        --missing: Pawns can only move diagonal if they can hit something
        --missing: Pawns can move 2 spaces if they are on their spawn

-- stopAtNearest takes positions -> stoppers -> positions
-- For my use it needs to be flipped
moveFilter :: Board -> [Pos] -> [Pos]            
moveFilter b = (flip stopAtNearest) (takenPositions b) . filter onBoard

allMoves :: Board -> Player -> [Board]
allMoves b p = filter (flip check p) $ clearMaybeBoard (concat $ map (moves b) fs)
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
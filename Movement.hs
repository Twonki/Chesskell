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
canAttack a b = p a == p b

moveTo :: Board -> ChessFigure -> Pos -> Board
moveTo b f p = (draw f p) : b''
    where   b' = filter (\n-> n /= f) b 
            e  = getFigOnPos b' p
            b'' = case e of 
                Nothing -> b' 
                Just e -> filter (\n-> n /= e) b'

            
moves :: Board -> ChessFigure -> [Board]
moves b fig@(ChessFigure t p c) =  map (moveTo b fig) possibleMoves
    where b' = filter (\n-> n /= fig) b  
          takenPositions = map (\t->pos t) b
          filter' = (stopAtNearest takenPositions) . (filter onBoard)
          routine = (concat . map filter' .  map ($p)) 
          possibleMoves = 
            case t  of  
              Bishop -> routine [risingDigL,fallingDigR,risingDigR,fallingDigL]
              Tower ->  routine [ups,downs,lefts,rights]
              Queen ->  routine $ [ups,downs,lefts,rights] ++ [risingDigL,fallingDigR,risingDigR,fallingDigL]
              King ->   routine [kingMoves]
              Knight -> routine [knightMoves]
              otherwise -> filter' $ peasantMoves p c

        --missing: Don't hit friends!
        --missing: Peasants can only move diagonal if they can hit something

allMoves :: Board -> Player -> [Board]
allMoves b p = concat $ map (moves b) fs
    where fs = getFigsForPlayer b p 

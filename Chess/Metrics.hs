module Chess.Metrics (
    simple,
    ratedSimple,
    agility
) where 

import Chess.Figures
import Chess.Movement

type Value = Double 


-- Simply counts figures on the board
-- Every figure has the same Value
simple :: Board -> Player -> Value
simple b p
    | p == W  = whites / blacks
    | p == B  = blacks / whites
    where 
        whites = fromIntegral $ length $ getFigsForPlayer b W
        blacks = fromIntegral $ length $ getFigsForPlayer b B

-- Each Figure has a certain value (see beyond) and the values are summed
ratedSimple :: Board -> Player -> Value
ratedSimple b p 
    | p == W = whiteValue / blackValue
    | p == B = blackValue / whiteValue
    where
        whiteValue = boardValue $ getFigsForPlayer b W 
        blackValue = boardValue $ getFigsForPlayer b B 

boardValue :: Board -> Value 
boardValue b = sum $ map value' $ map (\x -> typ x) b 

value' :: Figure -> Value
value' Pawn = 1
value' Knight = 3
value' Bishop = 3 
value' Tower = 3 
value' Queen = 7 -- Queens Value is one more than twice the tier 2 units
value' King = 34 -- Kings Value is one more than the whole Board  

-- The achievable moves are counted 
-- The more moves you can do, the better your board is
agility :: Board -> Player -> Value 
agility b p 
    | p == W = whiteMoves / blackMoves
    | p == B = blackMoves / whiteMoves
    where 
        whiteMoves = fromIntegral $ length $ allMoves b W 
        blackMoves = fromIntegral $ length $ allMoves b B 
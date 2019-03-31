module Chess.CoreMovement where 

import Chess.Figures

import  Data.List(minimumBy)
import  Data.Function (on)

-- Predicate whether the Pos is on the ChessBoard
onBoard :: Pos -> Bool 
onBoard (x,y) = elem x reach && elem y reach

reach = [1..8]
reach' :: [Pos]
reach' = zipWith (,) reach reach

line ::(Pos->Pos->Pos) -> Pos -> [Pos]
line f p = map (f p) reach' 

-- All positions moving 0 +
ups :: Pos -> [Pos]
ups = line (\(x,y)-> \(i,_)->(x,y+i))
-- All positions moving 0 - 
downs :: Pos -> [Pos] 
downs = line (\(x,y)-> \(i,_)->(x,y-i))
-- All Positions moving - 0
lefts :: Pos -> [Pos]
lefts  = line (\(x,y)-> \(i,_)->(x-i,y))
-- All positions moving + 0 
rights :: Pos -> [Pos]
rights = line (\(x,y)-> \(i,_)->(x+i,y))
-- All Positions moving - +
risingDigL :: Pos -> [Pos]
risingDigL = line (\(x,y)-> \(a,b)->(x-a,y+b))
-- All Positions moving + +
risingDigR :: Pos -> [Pos]
risingDigR = line (\(x,y)-> \(a,b)->(x+a,y+b))
-- All positions moving - - 
fallingDigL :: Pos -> [Pos]
fallingDigL = line (\(x,y)-> \(a,b)->(x-a,y-b)) 
-- All positions moving + -  
fallingDigR :: Pos -> [Pos]
fallingDigR  = line (\(x,y)-> \(a,b)->(x+a,y-b))

-- This function stops at (x,y) if it is in the positionlist
-- The Point (x,y) is still included
-- if (x,y) is not in the list, the list is returned
stopAt :: [Pos] -> Pos -> [Pos]
stopAt [] _ = []
stopAt (x:xs) b = if x==b then [x] else x : stopAt xs b

-- As there cannot be two items on the same spot
-- It's enough to check every pos and get the shortest one
stopAtNearest :: [Pos] -> [Pos] -> [Pos]
stopAtNearest xs ss = minimumBy (compare`on`length) $ map (stopAt xs) ss

knightMoves :: Pos -> [Pos]
knightMoves (x,y) = [(x+dx,y+dy) | dx <-[1,(-1),2,(-2)] , dy <- [1,(-1),2,(-2)], distance' dx dy==3]

kingMoves :: Pos -> [Pos]
kingMoves (x,y) = [(x+dx,y+dy) | dx <- [0,1,(-1)],dy <- [0,1,(-1)], distance' dx dy == 1]

pawnMoves :: Pos -> Player -> [Pos]
pawnMoves (x,y) t = map (add (x,y)) $ filter (\x->jumpWidth x <= 2) $ (,) <$> [-1,0,1] <*> vert
    where 
        vert
            | t == W && y == 2   = [1,2]
            | t == W             = [1]
            | t == B && y == 7   = [-1,-2]
            | otherwise          = [-1]

add (x,y) (a,b) = (x+a,y+b)
jumpWidth (a,b) = distance' a b
distance' a b = abs(a)+abs(b)
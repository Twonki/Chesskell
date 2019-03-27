import  Data.List.Split (splitOn)
import  Data.List(minimumBy)
import  Data.Function (on)
-- represents a single figure of chess
-- it gets a char for the player, and a position (x,y)
data Figure = Peasant
                | Knight
                | Bishop
                | Tower 
                | Queen
                | King
                deriving (Eq,Show,Read)
                            
type Pos = (Int,Int)

data ChessFigure = ChessFigure {
        typ :: Figure,
        pos :: Pos, 
        p   :: Char
    } deriving (Eq)

type Board = [ChessFigure]

getFigOnPos :: Board -> Pos -> Maybe ChessFigure
getFigOnPos b t = 
        if candidate == []
        then Nothing 
        else Just $ head candidate
    where candidate = filter (\(ChessFigure a b c) -> b == t) b

getFigsForPlayer :: Board -> Char -> [ChessFigure]
getFigsForPlayer b d = filter (\(ChessFigure a b c) -> d == c) b

instance Show ChessFigure where 
    show c = "("++show (typ c) ++ "," ++ show (pos c) ++ "," ++ [(p c)] ++")"

movePattern :: Figure -> Char -> [Pos]
movePattern f t = 
    case f of 
         Knight -> [(dx,dy) | dx <-[1,(-1),2,(-2)] , dy <- [1,(-1),2,(-2)], distance dx dy==3]
         King -> [(dx,dy) | dx <- [0,1,(-1)],dy <- [0,1,(-1)], distance dx dy == 1]
         Tower -> zipWith (,) (cycle [0]) [-8..8] ++ zipWith (,) [-8..8] (cycle [0])
         Bishop -> map (\n -> (n,n)) [-8..8] ++ map (\n -> (n,-n)) [-8..8]
         Queen -> (movePattern Tower t) ++ (movePattern Bishop t) 
         otherwise -> if t == 'w' 
                      then [(0,-1),(-1,-1),(1,-1)] --White Peasants move down
                      else [(0,1),(-1,1),(1,1)]    --Black Peasants move up
    where distance a b = abs(a)+abs(b)

-- Makes a List of Boards which are reachable from the current Board
nextBoards :: Board -> Char -> [Board]
nextBoards b c = concat $ map (\n -> validMoves n b) b'
            where b' = getFigsForPlayer b c

-- Performs for a single ChessFigure every possible move (filtered if they are valid)
validMoves :: ChessFigure -> Board -> [Board]
validMoves f b = map (:b') allnewPos  
    where
        b' = filter (\t-> t /= f) b  
        (x,y) = pos f
        allMoves = map (\(u,v)->(u+x,v+y)) $  movePattern (typ f) (p f)
        boardMoves = filter (\t -> onBoard t) allMoves
        allnewPos = map (moveTo f) boardMoves

-- A Chessfigure can move "onto" another Chessfigure if it has a different colour
canAttack :: ChessFigure -> ChessFigure -> Bool
canAttack a b = p a == p b

-- Towers, Queens and Bishops cannot "push through" an enemy. They're movements are limited when they "hit" an enemy
isStopped :: ChessFigure -> Board -> Bool
isStopped = undefined

collision :: [Pos] -> ChessFigure -> [Pos]
collision [] _ = []
collision l (ChessFigure _ cp c)
    | not (cp `elem` l) = l 
    | otherwise = head l'
        where l' = splitOn [cp] l 


moveTo :: ChessFigure -> Pos -> ChessFigure
moveTo f t = ChessFigure {typ = (typ f),pos=t,p=(p f)}

-- ===========================================
-- Movement Area
-- ===========================================

-- Predicat whether the Pos is on the ChessBoard
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

--TODO on ChessLevel: I get now every reachable item, last step: Dropout the ones which are "friendly" with a simple filter
--I can do this globally for every Unit


initialBoard :: Board 
initialBoard = 
    [ChessFigure Peasant (x,7) 'w' | x <- [1..8]] 
    ++ [ChessFigure Peasant (x,2) 'b' | x <- [1..8]]
    ++ [ChessFigure Tower (1,1) 'b',ChessFigure Tower (8,1) 'b',ChessFigure Tower (1,8) 'w',ChessFigure Tower (8,8) 'w']
    ++ [ChessFigure Knight (2,1) 'b',ChessFigure Knight (7,1) 'b',ChessFigure Knight (2,8) 'w',ChessFigure Knight (7,8) 'w']
    ++ [ChessFigure Bishop (3,1) 'b',ChessFigure Bishop (6,1) 'b',ChessFigure Bishop (3,8) 'w',ChessFigure Bishop (6,8) 'w']
    ++ [ChessFigure Queen (4,1) 'b' , ChessFigure Queen (5,8) 'w']
    ++ [ChessFigure King (5,1) 'b' , ChessFigure King (4,8) 'w']
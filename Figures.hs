import  Data.List.Split (splitOn)
import  Data.List(minimumBy)
import  Data.Function (on)

data Figure = Peasant
                | Knight
                | Bishop
                | Tower 
                | Queen
                | King
                deriving (Eq,Show,Read)
                            
type Pos = (Int,Int)
type Player = Char

data ChessFigure = ChessFigure {
        typ :: Figure,
        pos :: Pos, 
        p   :: Player
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

-- A Chessfigure can move "onto" another Chessfigure if it has a different colour
canAttack :: ChessFigure -> ChessFigure -> Bool
canAttack a b = p a == p b

moveTo :: Board -> ChessFigure -> Pos -> Board
moveTo b f p = (draw f p) : b''
    where b' = filter (\n-> n /= f) b 
          e  = getFigOnPos b' p
          b'' = case e of 
            Nothing -> b' 
            Just e -> filter (\n-> n /= e) b'


draw :: ChessFigure -> Pos -> ChessFigure
draw f t = ChessFigure {typ = (typ f),pos=t,p=(p f)}
            

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
-- ===========================================
-- Core Movement Area
-- ===========================================

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

peasantMoves :: Pos -> Player -> [Pos]
peasantMoves (x,y) t = 
    if t == 'w' 
        then [(x,y-1),(x-1,y-1),(x+1,y-1)] --White Peasants move down
        else [(x,y+1),(x-1,y+1),(x+1,y+1)]    --Black Peasants move up

distance' a b = abs(a)+abs(b)

initialBoard :: Board 
initialBoard = 
    [ChessFigure Peasant (x,7) 'w' | x <- [1..8]] 
    ++ [ChessFigure Peasant (x,2) 'b' | x <- [1..8]]
    ++ [ChessFigure Tower (1,1) 'b',ChessFigure Tower (8,1) 'b',ChessFigure Tower (1,8) 'w',ChessFigure Tower (8,8) 'w']
    ++ [ChessFigure Knight (2,1) 'b',ChessFigure Knight (7,1) 'b',ChessFigure Knight (2,8) 'w',ChessFigure Knight (7,8) 'w']
    ++ [ChessFigure Bishop (3,1) 'b',ChessFigure Bishop (6,1) 'b',ChessFigure Bishop (3,8) 'w',ChessFigure Bishop (6,8) 'w']
    ++ [ChessFigure Queen (4,1) 'b' , ChessFigure Queen (5,8) 'w']
    ++ [ChessFigure King (5,1) 'b' , ChessFigure King (4,8) 'w']

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

-- Performs for a single ChessFigure every possible move (filtered if they are valid)
validMoves :: ChessFigure -> Board -> [Board]
validMoves f b = map (:b') allnewPos  
    where
        b' = filter (\t-> t /= f) b  
        (x,y) = pos f
        allMoves = map (\(u,v)->(u+x,v+y)) $  movePattern (typ f) (p f)
        boardMoves = filter (\t -> onBoard t) allMoves
        allnewPos = map (moveTo f) boardMoves

onBoard :: Pos -> Bool 
onBoard (x,y) = elem x [1..8] && elem y [1..8]

-- A Chessfigure can move "onto" another Chessfigure if it has a different colour
canHit :: ChessFigure -> ChessFigure -> Bool
canHit a b = p a == p b

-- Towers, Queens and Bishops cannot "push through" an enemy. They're movements are limited when they "hit" an enemy
isStopped :: ChessFigure -> Board -> Bool
isStopped = undefined

moveTo :: ChessFigure -> Pos -> ChessFigure
moveTo f t = ChessFigure {typ = (typ f),pos=t,p=(p f)}
module Chess.Figures where 

data Figure = Pawn
                | Knight
                | Bishop
                | Tower 
                | Queen
                | King
                deriving (Eq,Show,Read)
                            
type Pos = (Int,Int)
data Player = W | B deriving (Eq,Show,Read)

data ChessFigure = ChessFigure {
        typ :: Figure,
        pos :: Pos, 
        player   :: Player
    } deriving (Eq)

type Board = [ChessFigure]

getFigOnPos :: Board -> Pos -> Maybe ChessFigure
getFigOnPos b t = 
        if candidate == []
        then Nothing 
        else Just $ head candidate
    where candidate = filter (\(ChessFigure a b c) -> b == t) b

getFigsForPlayer :: Board -> Player -> [ChessFigure]
getFigsForPlayer b d = filter (\(ChessFigure a b c) -> d == c) b

instance Show ChessFigure where 
    show c = "("++show (typ c) ++ "," ++ show (pos c) ++ "," ++ show (player c) ++")"

changePlayer :: Player -> Player 
changePlayer W = B 
changePlayer B = W

--Sets a Chessfigure to a new pos 
-- Regardless whether it's allowed to perform this move or if there is anything
draw :: ChessFigure -> Pos -> ChessFigure
draw f t = ChessFigure {typ = (typ f),pos=t,player=(player f)}

takenPositions :: Board -> [Pos]
takenPositions b = map (\t->pos t) b

hasKing :: [ChessFigure] -> Bool 
hasKing = foldr (||) False . map (\(ChessFigure f _ _) -> f == King)

-- A Chessfigure can move "onto" another Chessfigure if it has a different colour
canAttack :: ChessFigure -> ChessFigure -> Bool
canAttack a b = player a /= player b

-- i declare draw if there are exactly two kings left
remie :: Board -> Bool 
remie b = length b == 2 && hasKing b && hasKing b'
    where (_:b') = b

initialBoard :: Board 
initialBoard = 
    [ChessFigure Pawn (x,7) W | x <- [1..8]] 
    ++ [ChessFigure Pawn (x,2) B | x <- [1..8]]
    ++ [ChessFigure Tower (1,1) B,ChessFigure Tower (8,1) B,ChessFigure Tower (1,8) W,ChessFigure Tower (8,8) W]
    ++ [ChessFigure Knight (2,1) B,ChessFigure Knight (7,1) B,ChessFigure Knight (2,8) W,ChessFigure Knight (7,8) W]
    ++ [ChessFigure Bishop (3,1) B,ChessFigure Bishop (6,1) B,ChessFigure Bishop (3,8) W,ChessFigure Bishop (6,8) W]
    ++ [ChessFigure Queen (4,1) B , ChessFigure Queen (5,8) W]
    ++ [ChessFigure King (5,1) B , ChessFigure King (4,8) W]
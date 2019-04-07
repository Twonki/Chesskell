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

data Chesspiece = Chesspiece {
        typ      :: Figure,
        pos      :: Pos, 
        player   :: Player
    } deriving (Eq)

type Board = [Chesspiece]

getFigOnPos :: Board -> Pos -> Maybe Chesspiece
getFigOnPos b t = 
        if candidate == []
        then Nothing 
        else Just $ head candidate
    where candidate = filter (\(Chesspiece a b c) -> b == t) b

getFigsForPlayer :: Board -> Player -> [Chesspiece]
getFigsForPlayer b d = filter (\(Chesspiece a b c) -> d == c) b

instance Show Chesspiece where 
    show c = "("++show (typ c) ++ "," ++ show (pos c) ++ "," ++ show (player c) ++")"

changePlayer :: Player -> Player 
changePlayer W = B 
changePlayer B = W

--Sets a Chesspiece to a new pos 
-- Regardless whether it's allowed to perform this move or if there is anything
draw :: Chesspiece -> Pos -> Chesspiece
draw f t = Chesspiece {typ = (typ f),pos=t,player=(player f)}

takenPositions :: Board -> [Pos]
takenPositions b = map (\t->pos t) b

hasKing :: [Chesspiece] -> Bool 
hasKing = foldr (||) False . map (\(Chesspiece f _ _) -> f == King)

-- A Chesspiece can move "onto" another Chesspiece if it has a different colour
canAttack :: Chesspiece -> Chesspiece -> Bool
canAttack a b = player a /= player b

-- i declare draw if there are exactly two kings left
remie :: Board -> Bool 
remie b = length b == 2 && hasKing b && hasKing b'
    where (_:b') = b

free :: Pos -> Board -> Bool 
free p b =
        case t of 
        Nothing -> True 
        otherwise -> False
    where t = getFigOnPos b p  

initialBoard :: Board 
initialBoard = 
    [Chesspiece Pawn (x,7) W | x <- [1..8]] 
    ++ [Chesspiece Pawn (x,2) B | x <- [1..8]]
    ++ [Chesspiece Tower (1,1) B,Chesspiece Tower (8,1) B,Chesspiece Tower (1,8) W,Chesspiece Tower (8,8) W]
    ++ [Chesspiece Knight (2,1) B,Chesspiece Knight (7,1) B,Chesspiece Knight (2,8) W,Chesspiece Knight (7,8) W]
    ++ [Chesspiece Bishop (3,1) B,Chesspiece Bishop (6,1) B,Chesspiece Bishop (3,8) W,Chesspiece Bishop (6,8) W]
    ++ [Chesspiece Queen (4,1) B , Chesspiece Queen (5,8) W]
    ++ [Chesspiece King (5,1) B , Chesspiece King (4,8) W]
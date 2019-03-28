module Figures where 

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


draw :: ChessFigure -> Pos -> ChessFigure
draw f t = ChessFigure {typ = (typ f),pos=t,p=(p f)}
            

initialBoard :: Board 
initialBoard = 
    [ChessFigure Peasant (x,7) 'w' | x <- [1..8]] 
    ++ [ChessFigure Peasant (x,2) 'b' | x <- [1..8]]
    ++ [ChessFigure Tower (1,1) 'b',ChessFigure Tower (8,1) 'b',ChessFigure Tower (1,8) 'w',ChessFigure Tower (8,8) 'w']
    ++ [ChessFigure Knight (2,1) 'b',ChessFigure Knight (7,1) 'b',ChessFigure Knight (2,8) 'w',ChessFigure Knight (7,8) 'w']
    ++ [ChessFigure Bishop (3,1) 'b',ChessFigure Bishop (6,1) 'b',ChessFigure Bishop (3,8) 'w',ChessFigure Bishop (6,8) 'w']
    ++ [ChessFigure Queen (4,1) 'b' , ChessFigure Queen (5,8) 'w']
    ++ [ChessFigure King (5,1) 'b' , ChessFigure King (4,8) 'w']
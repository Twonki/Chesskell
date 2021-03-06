module Chess.Figures where 

import Data.Maybe(isNothing)
import Control.Monad(join)

data Figure = Pawn | Knight | Bishop | Tower | Queen | King
    deriving (Eq,Show,Read)
                            
type Pos = (Int,Int)
data Player = W | B deriving (Eq,Show,Read)

data Chesspiece = Chesspiece {
        typ      :: Figure,
        pos      :: Pos, 
        player   :: Player
    } deriving (Eq)

type Board = [Chesspiece]

pieceOnPos :: Board -> Pos -> Maybe Chesspiece
pieceOnPos b t = 
    if null candidate 
        then Nothing 
        else Just $ head candidate
    where candidate = filter (\cp -> pos cp == t) b

piecesForPlayer :: Board -> Player -> [Chesspiece]
piecesForPlayer b d = filter (\cp -> player cp == d) b

instance Show Chesspiece where 
    show c = "("++ smallColor (player c) ++shortName (typ c) ++ ")" -- LIVE 
    --show c = "("++ smallColor (player c) ++shortName (typ c) ++ "@"++ show (pos c)++")" -- DEBUG 

printBoard :: Board -> String 
printBoard b = header ++ boardPrint
    where 
        boardPrint = join [printLine b l ++ "|" ++ (show l) ++ "\n" | l<-[1..8]]
        header = " " ++ join [" _"++(show i)++"_ " | i <- [1..8]] ++ "\n"

printLine :: Board -> Int -> String 
printLine b l = join [printCell b (x,l)| x<-[1..8]]

printCell :: Board -> Pos -> String
printCell b p = 
    case mpiece of 
        Nothing -> "|____"
        Just n  -> "|"++ show n
    where mpiece = pieceOnPos b p 

changePlayer :: Player -> Player 
changePlayer W = B 
changePlayer B = W

-- Sets a Chesspiece to a new pos 
-- Regardless whether it's allowed to perform this move or if there is anything
draw :: Chesspiece -> Pos -> Chesspiece
draw f t = Chesspiece {typ = typ f ,pos=t,player= player f}

takenPositions :: Board -> [Pos]
takenPositions = map pos

removePiece :: Board -> Chesspiece -> Board
removePiece b p = filter (/=p) b  

hasKing :: [Chesspiece] -> Bool
hasKing = any (\(Chesspiece f _ _) -> f == King)

-- A Chesspiece can move "onto" another Chesspiece if it has a different colour
canAttack :: Chesspiece -> Chesspiece -> Bool
canAttack a b = player a /= player b

-- i declare draw if there are exactly two kings left
remie :: Board -> Bool 
remie b = length b == 2 && hasKing b && hasKing b'
    where (_:b') = b

free :: Pos -> Board -> Bool 
free p b = isNothing $ pieceOnPos b p 

missingPieces :: Board -> Player -> [Figure]
missingPieces b p = mask  difs pieces -- i select all figures where i don't have as many as i should have
    where
        numberIfFull:: Figure -> Int 
        numberIfFull f | f==Pawn = 8 | f==King = 1 | f==Queen = 1 | otherwise = 2
        pieces = [Pawn,Bishop,Knight,Tower,Queen,King] --a full set of figures
        pBoard = typ <$> piecesForPlayer b p 
        nums = (\x-> length (filter (==x) pBoard)) <$> pieces -- the amount of figures i have for each type
        expectedNums = numberIfFull <$> pieces -- the normal amount of figures a full set would have
        difs = zipWith (/=) nums expectedNums  -- Bool-List if i have as many figures as i could max have

initialBoard :: Board 
initialBoard = 
    [Chesspiece Pawn (x,7) W | x <- [1..8]] ++ [Chesspiece Pawn (x,2) B | x <- [1..8]]
    ++ [Chesspiece Tower (1,1) B,Chesspiece Tower (8,1) B,Chesspiece Tower (1,8) W,Chesspiece Tower (8,8) W]
    ++ [Chesspiece Knight (2,1) B,Chesspiece Knight (7,1) B,Chesspiece Knight (2,8) W,Chesspiece Knight (7,8) W]
    ++ [Chesspiece Bishop (3,1) B,Chesspiece Bishop (6,1) B,Chesspiece Bishop (3,8) W,Chesspiece Bishop (6,8) W]
    ++ [Chesspiece Queen (4,1) B , Chesspiece Queen (5,8) W] ++ [Chesspiece King (5,1) B , Chesspiece King (4,8) W]

mask :: [Bool] -> [a] -> [a]
mask bs xs = [x | (b,x) <- zip bs xs , b ]

shortName :: Figure -> String 
shortName f | f== Pawn = "P" | f == Bishop = "B" | f==Queen = "Q" | f==King = "K"| f==Knight = "H" | f==Tower = "T"

smallColor :: Player -> String 
smallColor c | c==W = "w" | c==B = "b"
module Chess.Figures where 

import Data.List (nub)

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

-- This was faulty: If only one Tower was missing, tower was not said to be missing. It was ony missing if every instance was gone.
--missingPieces :: Board -> Player -> [Figure]
--missingPieces b p = nub $ filter missing pBoardRef
--        where 
--            pBoard = map typ $ getFigsForPlayer b p
--            pBoardRef = map typ $ getFigsForPlayer initialBoard p
--            missing = (\x-> not $ x `elem` pBoard)


missingPieces :: Board -> Player -> [Figure]
missingPieces b p = mask pieces difs
    where
        pieces = [Pawn,Bishop,Knight,Tower,Queen,King]
        pBoard = map typ $ getFigsForPlayer b p
        nums = map (\x-> length (filter (\a->a == x) pBoard)) pieces
        expectedNums = map numberIfFull pieces
        difs = zipWith (/=) nums expectedNums 


numberIfFull:: Figure -> Int 
numberIfFull Pawn = 8
numberIfFull King = 1 
numberIfFull Queen = 1
numberIfFull _ = 2

initialBoard :: Board 
initialBoard = 
    [Chesspiece Pawn (x,7) W | x <- [1..8]] 
    ++ [Chesspiece Pawn (x,2) B | x <- [1..8]]
    ++ [Chesspiece Tower (1,1) B,Chesspiece Tower (8,1) B,Chesspiece Tower (1,8) W,Chesspiece Tower (8,8) W]
    ++ [Chesspiece Knight (2,1) B,Chesspiece Knight (7,1) B,Chesspiece Knight (2,8) W,Chesspiece Knight (7,8) W]
    ++ [Chesspiece Bishop (3,1) B,Chesspiece Bishop (6,1) B,Chesspiece Bishop (3,8) W,Chesspiece Bishop (6,8) W]
    ++ [Chesspiece Queen (4,1) B , Chesspiece Queen (5,8) W]
    ++ [Chesspiece King (5,1) B , Chesspiece King (4,8) W]

mask :: [a] -> [Bool] -> [a]
mask [] [] = []
mask a@(ah:as) b@(bh:bs)  
    | length a /= length b = error "missmatch in masking"
    | otherwise = if bh then ah:(mask as bs) else mask as bs  
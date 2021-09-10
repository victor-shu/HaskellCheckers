module Game where

import System.IO
import Data.List
import Data.Char

data PType = Norm | King deriving Eq
data Color = W | B deriving Eq
data Piece = Empty | Piece {pt :: PType, color :: Color} deriving Eq
type Board = [[Piece]]

type Turn = Color

type Pos = (Int, Int)
data Move = Error | Move {start :: Pos, end :: Pos} deriving Show

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (map f)

replace2d :: Pos -> [[a]] -> a -> [[a]]
replace2d p b a = replace (fst p) b $ replace (snd p) (b !! fst p) a

replace :: Int -> [a] -> a -> [a]
replace index list e = take index list ++ (e : drop (index + 1) list)

toggleTurn :: Turn -> Turn
toggleTurn W = B
toggleTurn B = W

correctColor :: Turn -> Piece -> Bool
correctColor _ Empty = False
correctColor W (Piece _ W) = True
correctColor B (Piece _ B) = True
correctColor t p = False

-- ex. 11 22
parseInput :: String -> Move
parseInput a
    | length a /= 5 = Error
    | inBounds move = move
    | otherwise = Error
        where move = Move pos1 pos2
              pos1 = ((\x -> x-1) $ digitToInt (head a), (\x -> x-1) $ digitToInt (a !! 1))
              pos2 = ((\x -> x-1) $ digitToInt (a !! 3), (\x -> x-1) $ digitToInt (a !! 4))

printBoard :: Board -> IO ()
printBoard a = putStrLn $ boardToString a

boardToString :: Board -> String
boardToString a = unlines $ addCoordinates $ flipBoard $ map unwords (map2d convertPiece (transpose a))

addCoordinates :: [String] -> [String]
addCoordinates b = (++ ["_ 1 2 3 4 5 6 7 8"]) $ zipWith (:) s (map (' ':) b)
    where s = ['8', '7', '6', '5', '4', '3', '2', '1']

flipBoard :: [a] -> [a]
flipBoard = foldl (flip (:)) []

convertPiece :: Piece -> String
convertPiece Empty = "*"
convertPiece (Piece Norm W) = "w"
convertPiece (Piece Norm B) = "b"
convertPiece (Piece King W) = "W"
convertPiece (Piece King B) = "B"

getPiece :: Board -> Pos -> Piece
getPiece b p = (b !! fst p) !! snd p

midpos :: Pos -> Pos -> Pos
midpos a b = ((fst a + fst b) `div` 2, (snd a + snd b) `div` 2)

inBoundPos :: Pos -> Bool
inBoundPos p
    | fst p >= 0 && fst p <= 7  && snd p >= 0 && snd p <= 7 = True
    | otherwise = False

inBounds :: Move -> Bool
inBounds Error = False
inBounds move = inBoundPos (start move) && inBoundPos (end move)

isForward :: Piece -> Move -> Bool
isForward (Piece Norm W) m = snd (end m) - snd (start m) < 0
isForward (Piece Norm B) m = snd (end m) - snd (start m) > 0

surroundingPos :: Pos -> [Pos]
surroundingPos p = filter inBoundPos [(fst p + 1, snd p + 1), (fst p - 1, snd p + 1), (fst p + 1, snd p - 1), (fst p - 1, snd p - 1)]

surroundingCap :: Pos -> [Pos]
surroundingCap p = filter inBoundPos [(fst p + 2, snd p + 2), (fst p - 2, snd p + 2), (fst p + 2, snd p - 2), (fst p - 2, snd p - 2)]

validCapture :: Board -> Piece -> Move -> Bool
validCapture b Empty m = False
validCapture b (Piece Norm _) m
    | st == Empty || en /= Empty || mid == Empty = False
    | not $ isForward st m = False
    | color st == color mid = False
    | otherwise = True
    where st = getPiece b (start m)
          en = getPiece b (end m)
          mid = getPiece b (midpos (start m) (end m))
validCapture b (Piece King _) m
    | st == Empty || en /= Empty || mid == Empty = False
    | color st == color mid = False
    | otherwise = True
    where st = getPiece b (start m)
          en = getPiece b (end m)
          mid = getPiece b (midpos (start m) (end m))

--valid move not a capture
validMove :: Board -> Piece -> Move -> Bool
validMove b Empty m = False
validMove b (Piece Norm _) m
    | end m `notElem` surroundingPos (start m) = False
    | en /= Empty = False
    | not $ isForward st m = False
    | otherwise = True
    where st = getPiece b (start m)
          en = getPiece b (end m)
validMove b (Piece King _) m
    | end m `notElem` surroundingPos (start m) = False
    | en /= Empty = False
    | otherwise = True
    where st = getPiece b (start m)
          en = getPiece b (end m)

vMHelper :: Board -> Piece -> Pos -> Pos -> Bool
vMHelper b p p1 p2 = validMove b p (Move p1 p2)

vCHelper :: Board -> Piece -> Pos -> Pos -> Bool
vCHelper b p p1 p2 = validCapture b p (Move p1 p2)

possibleMoves :: Board -> Pos -> [Pos]
possibleMoves b p = filter (vMHelper b (getPiece b p) p) (surroundingPos p)

possibleCaptures :: Board -> Pos -> [Pos]
possibleCaptures b p = filter (vCHelper b (getPiece b p) p) (surroundingCap p)

allMoves :: Board -> Pos -> [Pos]
allMoves b p = possibleCaptures b p ++ possibleMoves b p

isMove :: Board -> Move -> Bool
isMove b m = end m `elem` possibleMoves b (start m)

isCapture :: Board -> Move -> Bool
isCapture b m = end m `elem` possibleCaptures b (start m)

promotion :: Piece -> Pos -> Piece
promotion p@(Piece Norm W) (_, 0) = Piece King W
promotion p@(Piece Norm B) (_, 7) = Piece King B
promotion a b = a

makeMove :: Board -> Turn -> Move -> Board
makeMove b t m
    | not $ correctColor t (getPiece b (start m)) = b 
    | isMove b m = replace2d (start m) (replace2d (end m) b (promotion (getPiece b (start m)) (end m))) Empty
    | isCapture b m = replace2d (start m) (replace2d (end m) (replace2d (midpos (start m) (end m)) b Empty) (promotion (getPiece b (start m)) (end m))) Empty
    | otherwise = b

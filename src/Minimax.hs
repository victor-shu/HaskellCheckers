module Minimax where

import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad

import Debug.Trace

import Game

partitionBoard :: Board -> Board
partitionBoard b = map fst [pnw, pnb, pkw, pkb]
    where p = partition (== Empty) (concat b)
          pnw = partition (== Piece Norm W) (snd p)
          pnb = partition (== Piece Norm B) (snd pnw)
          pkw = partition (== Piece King W) (snd pnb)
          pkb = partition (== Piece King B) (snd pkw)

calcValue :: Board -> Color -> Int
calcValue b c
    | c == W = v
    | c == B = v * (-1)
    where pb = partitionBoard b
          v = length (head pb) + (length (pb !! 2) * 2) - length (pb !! 1) - (length (pb !! 3) * 2)

generateAllMovesHelper :: Board -> Color -> [[Move]]
generateAllMovesHelper board c = do
    (x, row) <- enu board
    (y, piece) <- enu row
    guard (correctColor c piece)
    return $ map (Move (x, y)) (allMoves board (x, y))
        where enu = zip [0..]

generateAllMoves :: Board -> Color -> [Move]
generateAllMoves b c = concat $ generateAllMovesHelper b c

checkWin :: Board -> Maybe Color
checkWin b 
    | null (generateAllMoves b W) = Just B
    | null (generateAllMoves b B) = Just W
    | otherwise = Nothing

first :: ((a, b), b) -> a
first tup = fst (fst tup)

minimax :: Int -> Color -> Board -> (Int, Board)
minimax 0 c b = (calcValue b c, b)
minimax depth c b
    | isJust (checkWin b) = (calcValue b c, b)
    | c == W = (first max, snd max)
    | c == B = (first min, snd min)
    where children = map (makeMove b c) (generateAllMoves b c)
          minimaxed = map (minimax (depth - 1) (toggleTurn c)) children
          zipped = zip minimaxed children
          max = maximumBy (comparing first) zipped
          min = minimumBy (comparing first) zipped

--    | isJust (checkWin b) = (calcValue b c, b)
--trace ("Debug" ++ show depth ++ "\n" ++ boardToString (snd max)) 
          
        
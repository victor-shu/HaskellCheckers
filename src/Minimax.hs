module Minimax where

import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad
import System.Random

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
    
zipIB :: (Int, Board) -> Board -> (Int, Board)
zipIB a b = (fst a, b)
 
randElement :: [a] -> IO a
randElement list = do
    ind <- randomRIO (0, length list - 1)
    return $ list !! ind

same :: (Int, Board) -> (Int, Board) -> Bool
same a b = fst a == fst b

--minimax algorithm. make random move if same score
mm :: StdGen -> Int -> Color -> Board -> (Int, Board)
mm g depth c b
    | checkWin b == Just B = (-100, b)
    | checkWin b == Just W = (100, b)
    | depth == 0 = (calcValue b c, b)
    | c == W = maxs !! rndmax
    | c == B = mins !! rndmin
    where 
          (rnd, newg) = random g :: (Int, StdGen)
          children = map (makeMove b c) (generateAllMoves b c)
          minimaxed = map (mm newg (depth - 1) (toggleTurn c)) children
          zipped = zipWith zipIB minimaxed children
          sortedinc = sortOn fst zipped
          sorteddec = reverse sortedinc
          maxs = takeWhile (same (head sorteddec)) sorteddec
          mins = takeWhile (same (head sortedinc)) sortedinc
          (rndmax, newgen1) = randomR (0, length maxs - 1) g :: (Int, StdGen)
          (rndmin, newgen2) = randomR (0, length mins - 1) g :: (Int, StdGen)   
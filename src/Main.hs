module Main where

import System.IO
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import System.Random

import Game
import Minimax

initboard = [[Empty, Piece Norm B, Empty, Piece Norm B, Empty, Piece Norm B, Empty, Piece Norm B],
            [Piece Norm B, Empty, Piece Norm B, Empty, Piece Norm B, Empty, Piece Norm B, Empty],
            [Empty, Piece Norm B, Empty, Piece Norm B, Empty, Piece Norm B, Empty, Piece Norm B],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Piece Norm W, Empty, Piece Norm W, Empty, Piece Norm W, Empty, Piece Norm W, Empty],
            [Empty, Piece Norm W, Empty, Piece Norm W, Empty, Piece Norm W, Empty, Piece Norm W],
            [Piece Norm W, Empty, Piece Norm W, Empty, Piece Norm W, Empty, Piece Norm W, Empty]]

main :: IO ()
main = do
    g <- getStdGen
    gameloop g (transpose initboard) B >>= printBoard

gameloop :: StdGen -> Board -> Turn -> IO Board
gameloop g b B = do
    printBoard b
    putStrLn "black turn"
    move <- getLine
    let newboard = makeMove b B (parseInput move)
    if isJust (checkWin newboard) then
        return newboard
    else gameloop g newboard W
gameloop g b W = do
    printBoard b
    putStrLn "white turn"
    let newboard = snd (mm g 4 W b)
    if isJust (checkWin newboard) then
        return newboard
    else gameloop g newboard B
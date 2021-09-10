module Main where

import System.IO
import Data.List
import Data.Char

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

testboard = [[Empty, Empty, Piece Norm B, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Piece Norm W, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]

ded = [[Empty]]

main :: IO ()
main = do
    gameloop (transpose initboard) B >>= printBoard

gameloop :: Board -> Turn -> IO Board
gameloop b B = do
    printBoard b
    putStrLn "black turn"
    move <- getLine
    let newboard = makeMove b B (parseInput move)
    if newboard == b then
        gameloop newboard B
    else gameloop newboard W
gameloop b W = do
    printBoard b
    putStrLn "white turn"
    let newboard = snd (minimax 4 W b)
    if newboard == b then
        gameloop ded W
    else gameloop newboard B
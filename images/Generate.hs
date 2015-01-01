module Main where

import Game.Halma.Board
import Game.Halma.Board.Draw
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main :: IO ()
main = mainWith (drawBoard board defaultTeamColours :: Diagram B R2)
  where board = initialBoard SmallGrid twoPlayers
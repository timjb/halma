module Main (main) where

import Test.Framework
import qualified Game.Halma.Board.Tests

main :: IO ()
main = defaultMain [Game.Halma.Board.Tests.tests]
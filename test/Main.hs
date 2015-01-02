module Main (main) where

import Test.Framework
import qualified Game.Halma.Board.Tests
import qualified Game.Halma.Rules.Tests

main :: IO ()
main = defaultMain
  [ Game.Halma.Board.Tests.tests
  , Game.Halma.Rules.Tests.tests
  ]
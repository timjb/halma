{-# LANGUAGE FlexibleContexts #-}

module Game.Halma.Board.Draw (defaultTeamColours, drawBoard) where

import Game.Halma.Board
import Math.Geometry.Grid
import Diagrams.Prelude
import qualified Data.Map.Strict as M

defaultTeamColours :: Team -> Colour Double
defaultTeamColours North = yellow
defaultTeamColours South = red
defaultTeamColours Northwest = cyan
defaultTeamColours Northeast = orange
defaultTeamColours Southwest = pink
defaultTeamColours Southeast = blue

drawBoard
  :: Renderable (Path R2) b
  => HalmaBoard size
  -> (Team -> Colour Double)
  -> QDiagram b R2 (Option (Last (Int, Int)))
drawBoard halmaBoard teamColors =
    targets `atop`
    (mconcat
      [ pieces # lw ultraThin
      , circles 0.15 # fc gray # lw none
      , gridLines # lc gray # lw thin
      ] # value (Option Nothing))
  where dirX = unitX
        dirY = rotateBy (1/6) unitX
        toCoord (x, y) = p2 $ unr2 $ fromIntegral x *^ dirX ^+^ fromIntegral y *^ dirY
        justLast = Option . Just . Last
        clickTarget = circle 0.4 # lw none
        targets = position $ map (\f -> (toCoord f, clickTarget # value (justLast f))) fields
        grid = getGrid halmaBoard
        fields = indices grid
        circles r = position $ zip (map toCoord fields) $ repeat $ circle r
        gridLines =
          mconcat $ map (\(p, q) -> fromVertices [toCoord p, toCoord q]) $
          concatMap (\f -> map ((,) f) $ filter (>= f) (neighbours grid f)) fields
        pieces = position $ map (\(p, t) -> (toCoord p, circle 0.25 # fc (teamColors t) # lc (darken 0.5 (teamColors t))))
                          $ M.toList $ toMap halmaBoard


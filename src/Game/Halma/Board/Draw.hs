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
  -> Diagram b R2
drawBoard halmaBoard teamColors =
           (pieces # lw ultraThin)
    `atop` (circles # fc gray # lw none)
    `atop` (gridLines # lc gray # lw thin)
  where dirX = unitX
        dirY = rotateBy (1/6) unitX
        toCoord (x, y) = p2 $ unr2 $ fromIntegral x *^ dirX ^+^ fromIntegral y *^ dirY
        grid = getGrid halmaBoard
        fields = indices grid
        circles = position $ zip (map toCoord fields) $ repeat $ circle 0.15
        gridLines =
          mconcat $ map (\(p, q) -> fromVertices [toCoord p, toCoord q]) $
          concatMap (\f -> map ((,) f) $ filter (>= f) (neighbours grid f)) fields
        pieces = position $ map (\(p, t) -> (toCoord p, circle 0.25 # fc (teamColors t) # lc (darken 0.5 (teamColors t))))
                          $ M.toList $ toMap halmaBoard


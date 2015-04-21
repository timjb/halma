{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Game.Halma.Board.Draw
  ( defaultTeamColours
  , drawBoard', drawBoard
  ) where

import Game.Halma.Board
import Math.Geometry.Grid
import Diagrams.Prelude

defaultTeamColours :: Team -> Colour Double
defaultTeamColours North = yellow
defaultTeamColours South = red
defaultTeamColours Northwest = cyan
defaultTeamColours Northeast = orange
defaultTeamColours Southwest = pink
defaultTeamColours Southeast = blue

-- | Render the board using the helper function for drawing the fields.
-- Supports querying for field positions.
drawBoard'
  :: (V b ~ V2, N b ~ Double, Renderable (Path V2 Double) b)
  => HalmaGrid size
  -> ((Int,Int) -> Diagram b)
  -> QDiagram b V2 Double (Option (Last (Int, Int)))
drawBoard' grid drawField =
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
        fields = indices grid
        circles r = position $ zip (map toCoord fields) $ repeat $ circle r
        gridLines =
          mconcat $ map (\(p, q) -> fromVertices [toCoord p, toCoord q]) $
          concatMap (\f -> map ((,) f) $ filter (>= f) (neighbours grid f)) fields
        pieces = position $ map (\p -> (toCoord p, drawField p)) fields

-- | Render the board using the given team colors. Supports querying for field
-- positions.
drawBoard
  :: (V b ~ V2, N b ~ Double, Renderable (Path V2 Double) b)
  => HalmaBoard size
  -> (Team -> Colour Double)
  -> QDiagram b V2 Double (Option (Last (Int, Int)))
drawBoard halmaBoard teamColors = drawBoard' (getGrid halmaBoard) drawField
  where drawPiece t =
          let c = teamColors t
          in circle 0.25 # fc c # lc (darken 0.5 c)
        drawField = maybe mempty drawPiece . flip lookupHalmaBoard halmaBoard


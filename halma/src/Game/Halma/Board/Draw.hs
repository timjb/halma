{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Game.Halma.Board.Draw
  ( defaultTeamColours
  , drawBoard', drawBoard
  ) where

import Game.Halma.Board

import Diagrams.Prelude
import Math.Geometry.Grid

defaultTeamColours :: Team -> Colour Double
defaultTeamColours team =
  -- colors from http://clrs.cc/
  case team of
    North     -> sRGB24read "#0074D9" -- blue
    Northeast -> sRGB24read "#2ECC40" -- green
    Northwest -> sRGB24read "#B10DC9" -- purple
    South     -> sRGB24read "#FF4136" -- red
    Southeast -> sRGB24read "#111111" -- black
    Southwest -> sRGB24read "#FF851B" -- orange

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
  where
    dirX = unitX
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
drawBoard halmaBoard teamColours = drawBoard' (getGrid halmaBoard) drawField
  where
    drawPiece piece =
      let c = teamColours (pieceTeam piece)
      in circle 0.25 # fc c # lc (darken 0.5 c)
    drawField = maybe mempty drawPiece . flip lookupHalmaBoard halmaBoard


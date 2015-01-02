{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Game.Halma.Board
import Game.Halma.Board.Draw
import Graphics.UI.Gtk
import Diagrams.Prelude
import Diagrams.TwoD.Size (requiredScale)
import Diagrams.Backend.Gtk
import Data.AffineSpace.Point
import Control.Monad.Trans (liftIO)

sizedCentered :: (Transformable a, Enveloped a, V a ~ R2) => SizeSpec2D -> a -> a
sizedCentered spec d = transform adjustT d
  where
    size = size2D d
    s    = requiredScale spec size
    finalSz = case spec of
      Dims w h -> (w,h)
      _        -> scale s size
    tr = (0.5 *. p2 finalSz) .-. (s *. center2D d)
    adjustT = translation tr <> scaling s

main :: IO ()
main = do
  _ <- initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  let board = initialBoard SmallGrid twoPlayers
      resizedFigure = do
        let figure = toGtkCoords $ drawBoard board defaultTeamColours # pad 1.05
        drawWin <- widgetGetDrawWindow canvas
        (w,h) <- drawableGetSize drawWin
        return $ sizedCentered (Dims (fromIntegral w) (fromIntegral h)) figure
      renderFigure = do
        win <- eventWindow
        liftIO $ resizedFigure >>= renderToGtk win
        return True
  _ <- canvas `on` sizeRequest $ return (Requisition 400 400)
  set window [ containerBorderWidth := 0, containerChild := canvas ]
  _ <- canvas `on` exposeEvent $ renderFigure
  _ <- onDestroy window mainQuit
  _ <- canvas `on` buttonPressEvent $ tryEvent $ do
    _click <- eventClick
    (x,y) <- eventCoordinates
    figure <- liftIO resizedFigure
    let result = runQuery (query figure) (P (r2 (x, y)))
    liftIO $ print result
  widgetShowAll window
  mainGUI

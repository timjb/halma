module Main (main) where

import Game.Halma.Board
import Game.Halma.Board.Draw
import Graphics.UI.Gtk
import Diagrams.Backend.Gtk
import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
  _ <- initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  --button <- buttonNew
  _ <- canvas `on` sizeRequest $ return (Requisition 256 256)
  set window [ containerBorderWidth := 10, containerChild := canvas ]
  _ <- canvas `on` exposeEvent $ renderFigure canvas
  --set button [ buttonLabel := "Hello World" ]
  --onClicked button (putStrLn "Hello World")
  _ <- onDestroy window mainQuit
  widgetShowAll window
  mainGUI

renderFigure :: DrawingArea -> EventM EExpose Bool
renderFigure canvas = do
  let board  = initialBoard SmallGrid twoPlayers
      figure = drawBoard board defaultTeamColours
  --win <- eventWindow
  --liftIO $ renderToGtk win $ toGtkCoords figure
  liftIO $ defaultRender canvas figure
  return True


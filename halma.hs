module Main (main) where

import Game.Halma.Board
import Game.Halma.Board.Draw
import Graphics.UI.Gtk
import Diagrams.Prelude
import Diagrams.Backend.Gtk
import Data.AffineSpace.Point
import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
  _ <- initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  let renderFigure = do
        --liftIO $ defaultRender canvas (toGtkCoords figure)
        win <- eventWindow
        liftIO $ renderToGtk win figure
        return True
  --button <- buttonNew
  _ <- canvas `on` sizeRequest $ return (Requisition 400 400)
  set window [ containerBorderWidth := 10, containerChild := canvas ]
  _ <- canvas `on` exposeEvent $ renderFigure
  --set button [ buttonLabel := "Hello World" ]
  --onClicked button (putStrLn "Hello World")
  _ <- onDestroy window mainQuit
  canvas `on` buttonPressEvent $ tryEvent $ do
    click <- eventClick
    (x,y) <- eventCoordinates
    let result = runQuery (query $ toGtkCoords figure) (P (r2 (x, y)))
    liftIO $ print result
  widgetShowAll window
  mainGUI
  where
    board  = initialBoard SmallGrid twoPlayers
    figure = toGtkCoords $ scale 20 $ drawBoard board defaultTeamColours

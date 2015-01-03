{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Game.Halma.Board
import Game.Halma.Board.Draw
import Graphics.UI.Gtk
import Diagrams.Prelude hiding ((<>))
import Diagrams.TwoD.Size (requiredScale)
import Diagrams.Backend.Gtk
import Data.AffineSpace.Point
import Control.Monad (forever)
import MVC
import Control.Monad.State.Strict (State)
import Control.Concurrent.Async (async, wait)
import qualified Data.Function as F
import Control.Concurrent.MVar

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

data HalmaState = HalmaState deriving (Eq, Show)

data ViewState = ViewState deriving (Eq, Show)

data ViewEvent = FieldClick (Int, Int)
               | EmptyClick

external :: Managed (View ViewState, Controller ViewEvent)
external = managed $ \f -> do
  _ <- initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  viewState <- newEmptyMVar
  let figure = do
        mvs <- tryReadMVar viewState
        let board = initialBoard SmallGrid twoPlayers
        return $ maybe mempty (const $ drawBoard board defaultTeamColours # pad 1.05) mvs
      resizedFigure = do
        drawWin <- widgetGetDrawWindow canvas
        sizedCentered <$> (uncurry (Dims `F.on` fromIntegral) <$> drawableGetSize drawWin)
                      <*> (toGtkCoords <$> figure)
      renderFigure = do
        win <- eventWindow
        liftIO $ resizedFigure >>= renderToGtk win
        return True
      updateViewState vs = do
        _ <- tryTakeMVar viewState
        putMVar viewState vs
        widgetQueueDraw window -- send redraw request to canvas
  (veOutput, veInput) <- spawn Single
  _ <- canvas `on` sizeRequest $ return (Requisition 400 400)
  set window [ containerBorderWidth := 0, containerChild := canvas ]
  _ <- canvas `on` exposeEvent $ renderFigure
  _ <- onDestroy window mainQuit
  _ <- canvas `on` buttonPressEvent $ tryEvent $ do
    _click <- eventClick
    (x,y) <- eventCoordinates
    fig <- liftIO resizedFigure
    let result = runQuery (query fig) (P (r2 (x, y)))
        event =
          case getOption result of
            Nothing -> EmptyClick
            Just (Last p) -> FieldClick p
    liftIO $ print result
    void $ liftIO $ atomically $ send veOutput event
  res <- async $ f (asSink updateViewState, asInput veInput)
  widgetShowAll window
  mainGUI
  wait res

pipe :: Pipe ViewEvent ViewState (State HalmaState) ()
pipe = forever $ yield ViewState >> await

main :: IO ()
main = void $ runMVC HalmaState (asPipe pipe) external

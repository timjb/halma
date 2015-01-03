{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Game.Halma.Board
import Game.Halma.Rules
import Data.Default
import Game.Halma.Board.Draw
import Graphics.UI.Gtk hiding (get)
import Diagrams.Prelude hiding ((<>))
import Diagrams.TwoD.Size (requiredScale)
import Diagrams.Backend.Gtk
import Data.AffineSpace.Point
import MVC
import Control.Monad.State.Strict
import Control.Concurrent.Async (async, wait)
import qualified Data.Function as F
import Control.Concurrent.MVar
import GHC.Conc (getNumCapabilities, setNumCapabilities)


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


data TurnCounter p = TurnCounter
  { _tcPlayers :: [p]
  , _tcCounter :: Int
  } deriving (Eq, Show)

newTurnCounter :: [p] -> TurnCounter p
newTurnCounter = flip TurnCounter 0

nextTurn :: TurnCounter p -> TurnCounter p
nextTurn (TurnCounter ps c) = TurnCounter ps (c+1)

currentPlayer :: TurnCounter p -> p
currentPlayer (TurnCounter ps c) = ps !! (c `mod` length ps)

_currentRound :: TurnCounter p -> Int
_currentRound (TurnCounter ps c) = c `div` length ps

data HalmaState size =
  HalmaState
  { hsRuleOptions :: RuleOptions
  , hsBoard :: HalmaBoard size
  , hsTurnCounter :: TurnCounter Team
  } deriving (Eq, Show)

initialState :: HalmaGrid size -> (Team -> Bool) -> HalmaState size
initialState grid players =
  HalmaState { hsRuleOptions = def
             , hsBoard = initialBoard grid players
             , hsTurnCounter = newTurnCounter (filter players [minBound..maxBound])
             }

data ViewState size =
  ViewState
  { _vsBoard :: HalmaBoard size
  , _vsSelectedField :: Maybe (Int, Int)
  , _vsHighlightedFields :: [(Int, Int)]
  } deriving (Eq, Show)

renderViewState
  :: Renderable (Path R2) b
  => (Team -> Colour Double)
  -> ViewState size
  -> QDiagram b R2 (Option (Last (Int, Int)))
renderViewState teamColors (ViewState board startPos highlighted) = drawBoard' (getGrid board) drawField
  where
    drawPiece t =
      let c = teamColors t
      in circle 0.25 # fc c # lc (darken 0.5 c)
    startField = startPos >>= flip lookupHalmaBoard board
    drawField p =
      (if Just p == startPos then lc black . lw thick else id) $
      case (lookupHalmaBoard p board, startField) of
        (Just t, _) -> drawPiece t
        (Nothing, Just t) | p `elem` highlighted ->
          drawPiece t # opacity 0.5
        _ -> mempty

data ViewEvent = FieldClick (Int, Int)
               | EmptyClick
               deriving (Eq, Show)


external :: Managed (View (ViewState size), Controller ViewEvent)
external = managed $ \f -> do
  _ <- initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  viewState <- newEmptyMVar
  let figure =
        fmap (maybe mempty (pad 1.05 . renderViewState defaultTeamColours))
             (tryReadMVar viewState)
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
        --putStrLn $ "updateViewState: " ++ show vs
        putMVar viewState vs
        widgetQueueDraw window -- send redraw request to canvas
        widgetQueueDraw window -- send redraw request to canvas
  (veOutput, veInput) <- spawn Single
  _ <- canvas `on` sizeRequest $ return (Requisition 450 450)
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
    liftIO $ print event
    void $ liftIO $ atomically $ send veOutput event
  res <- async $ f (asSink updateViewState, asInput veInput)
  widgetShowAll window
  mainGUI
  wait res

pipe :: Pipe ViewEvent (ViewState size) (State (HalmaState size)) ()
pipe = do
  HalmaState ruleOptions board turnCounter <- get
  let team = currentPlayer turnCounter
      noSelectionLoop = do
        yield $ ViewState board Nothing []
        event <- await
        case event of
          EmptyClick -> noSelectionLoop
          FieldClick p | lookupHalmaBoard p board == Just team ->
            selectionLoop p
          FieldClick _ -> noSelectionLoop
      selectionLoop startPos = do
        let possible = possibleMoves ruleOptions board startPos
        yield $ ViewState board (Just startPos) possible
        event <- await
        case event of
          EmptyClick -> noSelectionLoop
          FieldClick p | p `elem` possible -> do
            let Right board' = movePiece startPos p board
            put $ HalmaState ruleOptions board' (nextTurn turnCounter)
            pipe
          FieldClick p | lookupHalmaBoard p board == Just team ->
            selectionLoop p
          FieldClick _ -> noSelectionLoop
  noSelectionLoop

main :: IO ()
main = do
  -- we need at least two threads:
  -- * one for the GTK event loop
  -- * one for the MVC pipeline
  caps <- getNumCapabilities
  when (caps < 2) $ setNumCapabilities 2
  void $ runMVC (initialState SmallGrid twoPlayers) (asPipe pipe) external

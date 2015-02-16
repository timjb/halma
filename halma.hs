{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Game.Halma.Board
import Game.Halma.Rules
import Data.Default
import Game.Halma.Board.Draw
import Graphics.UI.Gtk hiding (get)
import Diagrams.Prelude hiding ((<>))
import Diagrams.TwoD.Size (requiredScale)
import Diagrams.TwoD.Text (Text)
import Diagrams.Backend.Gtk
import Diagrams.Backend.Cairo.Internal (Cairo)
import Data.AffineSpace.Point
import MVC
import qualified Pipes.Prelude as PP
import qualified Control.Monad.State.Strict as MS
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar
import GHC.Conc (getNumCapabilities, setNumCapabilities)
import System.TimeIt
import Control.Monad (when)
import qualified Data.Function as F


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

centered :: (Transformable a, Enveloped a, V a ~ R2) => SizeSpec2D -> a -> a
centered spec d = transform adjustT d
  where
    size = size2D d
    s    = requiredScale spec size
    finalSz = case spec of
      Dims w h -> (w,h)
      _        -> scale s size
    tr = (0.5 *. p2 finalSz) .-. center2D d
    adjustT = translation tr


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

data NumberOfPlayers :: HalmaGridSize -> * where
  TwoPlayers   :: NumberOfPlayers size
  ThreePlayers :: NumberOfPlayers size
  FourPlayers  :: NumberOfPlayers 'L
  FivePlayers  :: NumberOfPlayers 'L
  SixPlayers   :: NumberOfPlayers 'L

deriving instance Show (NumberOfPlayers size)

instance Eq (NumberOfPlayers size) where
  a == b = show a == show b

getPlayers :: NumberOfPlayers size -> [Team]
getPlayers TwoPlayers   = [North, South]
getPlayers ThreePlayers = [Northeast, South, Northwest]
getPlayers FourPlayers  = [Northeast, Southeast, Southwest, Northwest]
getPlayers FivePlayers  = [Northeast, Southeast, South, Southwest, Northwest]
getPlayers SixPlayers   = [minBound..maxBound]

data MenuState where
  MenuState :: HalmaGrid size -> NumberOfPlayers size -> MenuState

deriving instance Show MenuState

instance Eq MenuState where
  a == b = show a == show b

initialMenuState :: MenuState
initialMenuState = MenuState SmallGrid TwoPlayers

data State where
  State :: MenuState -> Maybe (HalmaState size) -> State

deriving instance Show State

newGame :: State -> State
newGame (State ms@(MenuState halmaGrid nop) _) = State ms (Just halmaState)
  where
    players = getPlayers nop
    halmaState = 
      HalmaState { hsRuleOptions = def
                 , hsBoard = initialBoard halmaGrid (flip elem players)
                 , hsTurnCounter = newTurnCounter players
                 }

initialState :: State
initialState = State initialMenuState Nothing

data HalmaViewState size =
  HalmaViewState
  { _hvsBoard :: HalmaBoard size
  , _hvsSelectedField :: Maybe (Int, Int)
  , _hvsHighlightedFields :: [(Int, Int)]
  } deriving (Eq, Show)

data ViewState where
  MenuView :: MenuState -> ViewState
  HalmaView :: HalmaViewState size -> ViewState

deriving instance Show ViewState

data QuitType = QuitGame | QuitApp deriving (Show, Eq)

data ViewEvent = Quit QuitType
               | SetMenuState MenuState
               | NewGame
               | FieldClick (Int, Int)
               | EmptyClick
               deriving (Eq, Show)

renderHalmaViewState
  :: Renderable (Path R2) b
  => (Team -> Colour Double)
  -> HalmaViewState size
  -> QDiagram b R2 (Option (Last (Int, Int)))
renderHalmaViewState teamColors (HalmaViewState board startPos highlighted) =
  drawBoard' (getGrid board) drawField
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

data ButtonState a = ButtonActive a
                   | ButtonInactive
                   | ButtonSelected deriving (Eq, Show)

button
  :: (Renderable (Path R2) b, Renderable Text b, Backend b R2)
  => String
  -> ButtonState a
  -> QDiagram b R2 (Option (Last a))
button txt buttonState = (label <> background) # value val' # padX 1.05 # padY 1.2
  where
    (label, background) = case buttonState of
      ButtonActive _ -> ( text txt # fontSizeO 13
                        , roundedRect 110 26 6 # fc lightgray
                        )
      ButtonInactive -> ( text txt # fontSizeO 13 # fc gray
                        , roundedRect 110 26 6 # fc lightgray # lc gray
                        )
      ButtonSelected -> ( text txt # fontSizeO 13
                        , roundedRect 110 26 6 # fc yellow # lc black # lw thick
                        )

    val' = case buttonState of
      ButtonActive val -> Option (Just (Last val))
      _ -> Option Nothing

renderMenu
  :: (Renderable (Path R2) b, Renderable Text b, Backend b R2)
  => MenuState
  -> QDiagram b R2 (Option (Last MenuState))
renderMenu (MenuState gridSize nop) =
  ((===) `F.on` (centerX . horizontal)) sizeButtons playerButtons
  where
    setPlayers = MenuState gridSize
    playerButtonAction nop' = if (nop == nop') then ButtonSelected else ButtonActive (setPlayers nop')
    horizontal = foldl (|||) mempty
    (sizeButtons, playerButtons) = case gridSize of
      SmallGrid ->
        let nopL = case nop of
                     TwoPlayers -> TwoPlayers
                     ThreePlayers -> ThreePlayers
                     _ -> error "impossible"
        in ( [ button "Small Grid" ButtonSelected
             , button "Large Grid" $ ButtonActive $ MenuState LargeGrid nopL
             ]
           , [ button "Two Players" $ playerButtonAction TwoPlayers
             , button "Three Players" $ playerButtonAction ThreePlayers
             ] ++ map (flip button ButtonInactive) ["Four Players", "Five Players", "Six Players"]
           )
      LargeGrid ->
        let nopS = case nop of
                     TwoPlayers -> TwoPlayers
                     _ -> ThreePlayers
        in ( [ button "Small Grid" $ ButtonActive $ MenuState SmallGrid nopS
             , button "Large Grid" ButtonSelected
             ]
           , map (\(numStr, nop') -> button (numStr ++ " Players") (playerButtonAction nop')) $
             [ ("Two", TwoPlayers), ("Three", ThreePlayers), ("Four", FourPlayers)
             , ("Five", FivePlayers), ("Six", SixPlayers)
             ]
           )

renderViewState
  :: (Team -> Colour Double)
  -> (Double, Double)
  -> ViewState
  -> QDiagram Cairo R2 (Option (Last ViewEvent))
renderViewState _teamColors (w,h) (MenuView menuState) =
  let menuDiagram   = fmap (fmap SetMenuState) <$> renderMenu menuState
      newGameButton = button "New Game" (ButtonActive NewGame) # padY 1.5
      reposition    = centered (Dims w h) . toGtkCoords
  in reposition (menuDiagram === newGameButton)
renderViewState teamColors (w,h) (HalmaView halmaViewState) =
  let resize = sizedCentered (Dims w h) . toGtkCoords . pad 1.05
      quitGameButton =
        toGtkCoords $ padY 1.3 $ alignX (-1) $
        button "Quit Game" $ ButtonActive $ Quit QuitGame
      field = resize (fmap (fmap FieldClick) <$> renderHalmaViewState teamColors halmaViewState)
  in quitGameButton `atop` field

external :: Managed (View ViewState, Controller ViewEvent)
external = managed $ \f -> do
  _ <- initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  set window [ containerBorderWidth := 0, containerChild := canvas ]

  viewState <- newEmptyMVar
  (veOutput, veInput) <- spawn (bounded 1)

  let figure winSize =
        fmap (maybe mempty (renderViewState defaultTeamColours winSize))
             (tryReadMVar viewState)
      resizedFigure = do
        drawWin <- widgetGetDrawWindow canvas
        (w, h) <- drawableGetSize drawWin
        figure (fromIntegral w, fromIntegral h)
      renderFigure = tryEvent $ do
        win <- eventWindow
        liftIO $ putStr "Render time: "
        liftIO $ timeIt $ resizedFigure >>= renderToGtk win
      updateViewState vs = do
        _ <- tryTakeMVar viewState
        putMVar viewState vs
        widgetQueueDraw canvas -- send redraw request to canvas
      handleClick = tryEvent $ do
        _click <- eventClick
        (x,y) <- eventCoordinates
        fig <- liftIO resizedFigure
        let result = runQuery (query fig) (P (r2 (x, y)))
            event = maybe EmptyClick getLast $ getOption result
        liftIO $ print event
        void $ liftIO $ atomically $ send veOutput event
      handleDestroy = do
        _ <- atomically $ send veOutput $ Quit QuitApp
        mainQuit

  _ <- canvas `on` sizeRequest $ return (Requisition 650 450)
  _ <- canvas `on` exposeEvent $ renderFigure
  _ <- canvas `on` buttonPressEvent $ handleClick
  _ <- window `onDestroy` handleDestroy

  res <- async $ f (asSink updateViewState, asInput veInput)
  widgetShowAll window
  mainGUI
  wait res

gameLoop :: HalmaState size -> Pipe ViewEvent (HalmaViewState size) (MS.State State) QuitType
gameLoop (HalmaState ruleOptions board turnCounter) = noSelectionLoop
  where
    team = currentPlayer turnCounter
    noSelectionLoop = do
      yield $ HalmaViewState board Nothing []
      event <- await
      case event of
        EmptyClick -> noSelectionLoop
        FieldClick p | lookupHalmaBoard p board == Just team ->
          selectionLoop p
        FieldClick _ -> noSelectionLoop
        Quit quitType -> return quitType
        _ -> return QuitApp
    selectionLoop startPos = do
      let possible = possibleMoves ruleOptions board startPos
      yield $ HalmaViewState board (Just startPos) possible
      event <- await
      case event of
        EmptyClick -> noSelectionLoop
        FieldClick p | p `elem` possible -> do
          let Right board' = movePiece startPos p board
              halmaState' = HalmaState ruleOptions board' (nextTurn turnCounter)
          State menuState _halmaState <- MS.get
          MS.put $ State menuState (Just halmaState')
          gameLoop halmaState'
        FieldClick p | lookupHalmaBoard p board == Just team ->
          selectionLoop p
        FieldClick _ -> noSelectionLoop
        Quit quitType -> return quitType
        _ -> return QuitApp

pipe :: Pipe ViewEvent ViewState (MS.State State) ()
pipe = do
  st@(State menuState mHalmaState) <- MS.get
  case mHalmaState of
    Just halmaState -> do
      quitType <- gameLoop halmaState >-> PP.map HalmaView
      when (quitType == QuitGame) $
        MS.put (State menuState Nothing) >> pipe
    Nothing -> do
      yield $ MenuView menuState
      event <- await
      case event of
        SetMenuState menuState' -> MS.put (State menuState' Nothing) >> pipe
        NewGame -> MS.put (newGame st) >> pipe
        _ -> pipe

main :: IO ()
main = do
  -- we need at least two threads:
  -- * one for the GTK event loop
  -- * one for the MVC pipeline
  caps <- getNumCapabilities
  when (caps < 2) $ setNumCapabilities 2
  void $ runMVC initialState (asPipe pipe) external

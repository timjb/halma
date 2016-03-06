{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
--{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Game.Halma.Board
import Game.Halma.Rules
import qualified Game.Halma.AI.Competitive as Competitive
import qualified Game.Halma.AI.Ignorant as Ignorant
import Game.Halma.Board.Draw
import Game.TurnCounter
import Game.Halma.State
import Game.Halma.Configuration

import Graphics.UI.Gtk hiding (get)
import Diagrams.Prelude hiding ((<>), set)
import Diagrams.TwoD.Text (Text)
import Diagrams.Backend.Gtk
import Diagrams.Backend.Cairo.Internal (Cairo)
import Data.Maybe (isJust)
import MVC
import qualified Pipes.Prelude as PP
import qualified Control.Monad.State.Strict as MS
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar
import GHC.Conc (getNumCapabilities, setNumCapabilities)
import System.TimeIt
import Control.Monad (when)
import qualified Data.Function as F

centered, sizedCentered
  :: (Transformable a, Enveloped a, V a ~ V2, N a ~ Double)
  => SizeSpec V2 Double -> a -> a
centered spec d = transform adjustT d
  where
    adjustT = translation $ (0.5 *. P (specToSize 0 spec)) .-. centerPoint d
sizedCentered spec = centered spec . sized spec

data State where
  State :: Configuration size -> Maybe (HalmaState size) -> State

deriving instance Show State

startNewGame :: State -> State
startNewGame (State config _) =
  State config (Just (newGame config))

initialState :: State
initialState = State defaultConfiguration Nothing

data HalmaViewState size =
  HalmaViewState
  { _hvsBoard :: HalmaBoard size
  , _hvsSelectedField :: Maybe (Int, Int)
  , _hvsHighlightedFields :: [(Int, Int)]
  , _hvsLastMoved :: Maybe (Int, Int)
  , _hvsFinishedPlayers :: [Team]
  , _hvsCompetitiveAIAllowed :: Bool
  } deriving (Eq, Show)

data ViewState where
  MenuView  :: Configuration size -> ViewState
  HalmaView :: HalmaViewState size -> ViewState

deriving instance Show ViewState

data QuitType = QuitGame | QuitApp deriving (Show, Eq)

data AIType = Ignorant | Competitive
  deriving (Eq, Show)

data ViewEvent = Quit QuitType
               | SetConfiguration SomeConfiguration
               | NewGame
               | FieldClick (Int, Int)
               | EmptyClick
               | AIMove AIType
               deriving (Eq, Show)

renderHalmaViewState
  :: (V b ~ V2, N b ~ Double, Renderable (Path V2 Double) b)
  => (Team -> Colour Double)
  -> HalmaViewState size
  -> QDiagram b V2 Double (Option (Last (Int, Int)))
renderHalmaViewState teamColors (HalmaViewState board startPos highlighted lastMoved _ _) =
  drawBoard' (getGrid board) drawField
  where
    drawPiece t lastMoved' =
      let c = teamColors t
      in circle 0.25 # fc c # lc (if lastMoved' then darken 0.2 c else darken 0.5 c) #
        if lastMoved' then lw medium else id
    startField = startPos >>= flip lookupHalmaBoard board
    drawField p =
      (if Just p == startPos then lc black . lw thick else id) $
      case (lookupHalmaBoard p board, startField) of
        (Just t, _) -> drawPiece t (Just p == lastMoved)
        (Nothing, Just t) | p `elem` highlighted ->
          drawPiece t False # opacity 0.5
        _ -> mempty

data ButtonState a = ButtonActive a
                   | ButtonInactive
                   | ButtonSelected deriving (Eq, Show)

button
  :: (Renderable (Path V2 Double) b, Renderable (Text Double) b)
  => String
  -> ButtonState a
  -> QDiagram b V2 Double (Option (Last a))
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

playerFinishedSign
  :: (Renderable (Path V2 Double) b, Renderable (Text Double) b)
  => Colour Double -> QDiagram b V2 Double (Option (Last a))
playerFinishedSign color = (label <> background) # value (Option Nothing) # padX 1.05 # padY 1.5
  where
    label = text "finished" # fontSizeO 13
    background = roundedRect 110 26 6 # fc color # lw none

renderMenu
  :: (Renderable (Path V2 Double) b, Renderable (Text Double) b)
  => Configuration size
  -> QDiagram b V2 Double (Option (Last SomeConfiguration))
renderMenu (Configuration gridSize nop) =
  ((===) `F.on` (centerX . horizontal)) sizeButtons playerButtons
  where
    setPlayers = SomeConfiguration . Configuration gridSize
    playerButtonAction nop' = if (nop == nop') then ButtonSelected else ButtonActive (setPlayers nop')
    horizontal = foldl (|||) mempty
    (sizeButtons, playerButtons) = allButtons
    allButtons
      :: (Renderable (Path V2 Double) b, Renderable (Text Double) b)
      => ( [ QDiagram b V2 Double (Option (Last SomeConfiguration)) ]
         , [ QDiagram b V2 Double (Option (Last SomeConfiguration)) ]
         )
    allButtons = case gridSize of
      SmallGrid ->
        let nopL = case nop of
                     TwoPlayers -> TwoPlayers
                     ThreePlayers -> ThreePlayers
                     _ -> error "impossible"
        in ( [ button "Small Grid" ButtonSelected
             , button "Large Grid" $ ButtonActive $ SomeConfiguration $ Configuration LargeGrid nopL
             ]
           , [ button "Two Players" $ playerButtonAction TwoPlayers
             , button "Three Players" $ playerButtonAction ThreePlayers
             ] ++ map (flip button ButtonInactive) ["Four Players", "Five Players", "Six Players"]
           )
      LargeGrid ->
        let nopS = case nop of
                     TwoPlayers -> TwoPlayers
                     _ -> ThreePlayers
        in ( [ button "Small Grid" $ ButtonActive $ SomeConfiguration $ Configuration SmallGrid nopS
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
  -> QDiagram Cairo V2 Double (Option (Last ViewEvent))
renderViewState _teamColors (w,h) (MenuView config) =
  let menuDiagram   = fmap (fmap SetConfiguration) <$> renderMenu config
      newGameButton = button "New Game" (ButtonActive NewGame) # padY 1.5
      reposition    = centered (dims (r2 (w, h))) . toGtkCoords
  in reposition (menuDiagram === newGameButton)
renderViewState teamColors (w,h) (HalmaView halmaViewState) =
  let resize = sizedCentered (dims (r2 (w, h))) . toGtkCoords . pad 1.05
      buttons = padY 1.3 $ quitGameButton === padY 1.3 aiButtons
      quitGameButton = button "Quit Game" $ ButtonActive $ Quit QuitGame
      aiButtons = button "AI Move" (aiButtonState Ignorant)
              === if _hvsCompetitiveAIAllowed halmaViewState
                  then button "Competitive" (aiButtonState Competitive) else mempty
      aiButtonState aiMoveType =
        if isJust (_hvsSelectedField halmaViewState)
        then ButtonInactive
        else ButtonActive $ AIMove aiMoveType
      finishedSigns = foldl (===) mempty $
        map (playerFinishedSign . teamColors) $ _hvsFinishedPlayers halmaViewState
      field = resize (fmap (fmap FieldClick) <$> renderHalmaViewState teamColors halmaViewState)
  in toGtkCoords (buttons === finishedSigns) `atop` field

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

gameLoop
  :: Configuration size
  -> HalmaState size
  -> Pipe ViewEvent (HalmaViewState size) (MS.State State) QuitType
gameLoop config (HalmaState ruleOptions board turnCounter lastMoved) = noSelectionLoop
  where
    team = currentPlayer turnCounter
    finishedPlayers = filter (hasFinished board) (tcPlayers turnCounter)
    noSelectionLoop = do
      yield $ HalmaViewState board Nothing [] lastMoved finishedPlayers
                             (length (tcPlayers turnCounter)==2)
      event <- await
      case event of
        EmptyClick -> noSelectionLoop
        FieldClick p | lookupHalmaBoard p board == Just team ->
          selectionLoop p
        FieldClick _ -> noSelectionLoop
        AIMove Ignorant -> performMove $
          Ignorant.aiMove ruleOptions board (currentPlayer turnCounter)
        AIMove Competitive -> performMove $
          Competitive.aiMove ruleOptions board
            (currentPlayer turnCounter, currentPlayer $ nextTurn turnCounter)
        Quit quitType -> return quitType
        _ -> return QuitApp
    selectionLoop startPos = do
      let possible = possibleMoves ruleOptions board startPos
      yield $ HalmaViewState board (Just startPos) possible Nothing finishedPlayers
                             (length (tcPlayers turnCounter)==2)
      event <- await
      case event of
        EmptyClick -> noSelectionLoop
        FieldClick p | p `elem` possible -> performMove (startPos, p)
        FieldClick p | lookupHalmaBoard p board == Just team ->
          selectionLoop p
        FieldClick _ -> noSelectionLoop
        Quit quitType -> return quitType
        _ -> return QuitApp
    performMove (source, destination) = do
      let Right board' = movePiece source destination board
          halmaState' = HalmaState ruleOptions board' (nextTurn turnCounter) (Just destination)
      MS.put $ State config (Just halmaState')
      gameLoop config halmaState'


pipe :: Pipe ViewEvent ViewState (MS.State State) ()
pipe = do
  st@(State config mHalmaState) <- MS.get
  case mHalmaState of
    Just halmaState -> do
      quitType <- gameLoop config halmaState >-> PP.map HalmaView
      when (quitType == QuitGame) $
        MS.put (State config Nothing) >> pipe
    Nothing -> do
      yield $ MenuView config
      event <- await
      case event of
        SetConfiguration (SomeConfiguration config') ->
          MS.put (State config' Nothing) >> pipe
        NewGame -> MS.put (startNewGame st) >> pipe
        _ -> pipe

main :: IO ()
main = do
  -- we need at least two threads:
  -- * one for the GTK event loop
  -- * one for the MVC pipeline
  caps <- getNumCapabilities
  when (caps < 2) $ setNumCapabilities 2
  void $ runMVC initialState (asPipe pipe) external

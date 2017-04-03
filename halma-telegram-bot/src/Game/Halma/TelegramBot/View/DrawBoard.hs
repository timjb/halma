{-# LANGUAGE FlexibleContexts #-}

module Game.Halma.TelegramBot.View.DrawBoard
  ( BoardLabels
  , withRenderedBoardInPngFile
  ) where

import Game.Halma.Board
import Game.Halma.Board.Draw
import Game.Halma.TelegramBot.Model.MoveCmd
import Game.Halma.TelegramBot.Model.Types

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)
import Diagrams.Prelude ((#), (|||), (*^), (&), (.~))
import Diagrams.Query (resetValue)
import Diagrams.Size (dims)
import Diagrams.TwoD.Types (V2 (..))
import System.Directory (getTemporaryDirectory)
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Diagrams.Prelude as D

type BoardLabels = M.Map (Int, Int) T.Text

type Backend = Rasterific

withRenderedBoardInPngFile
  :: (MonadIO m, MonadMask m)
  => HalmaState
  -> BoardLabels
  -> (FilePath -> m a)
  -> m a
withRenderedBoardInPngFile game labels action =
  withTempPngFilePath $ \path -> do
    let
      dia = drawBoardForChat game labels # D.centerXY # D.pad 1.1
      bounds = dims (V2 1000 1000)
    liftIO $ do
      t1 <- getCPUTime
      renderRasterific path bounds dia
      t2 <- getCPUTime
      let renderTimeInSec = fromIntegral (t2-t1) * 1e-12 :: Float
      printf "[note] Rendering the board took %.2fs in CPU time\n" renderTimeInSec
    action path
  where
    withTempPngFilePath handler = do
      systemTempDir <- liftIO getTemporaryDirectory
      withTempFile systemTempDir "halma.png" $ \filePath fileHandle -> do
        liftIO (hClose fileHandle)
        handler filePath

drawBoardForChat :: HalmaState -> BoardLabels -> D.Diagram Backend
drawBoardForChat game labels =
  let
    boardDia = resetValue (drawBoard' (getGrid board) drawField)
  in
    scale ||| D.strut D.unitX ||| boardDia ||| D.strut D.unitX ||| scale
  where
    board = hsBoard game
    dirY = D.rotateBy (1/6) D.unitX
    rowDir = dirY & D._x .~ 0
    boardFontStyle x = x # D.fontSize (D.output 22) # D.font "Arial"
    scale = D.position $ map rowMarker [(-r)..r]
      where
        r = radiusInRows (getGrid board)
        rowMarker i = (rowPosition i, rowNumberLabel i)
        rowPosition i = D.p2 $ D.unr2 $ fromIntegral i *^ rowDir
        rowNumberLabel i =
          let
            humanRowNumber = internalToHumanRowNumber (getGrid board) i
            txt = show (unRowNumber humanRowNumber)
          in
            D.text txt # boardFontStyle # D.fc D.gray
    drawField :: (Int, Int) -> D.Diagram Backend
    drawField field =
      case lookupHalmaBoard field board of
        Just piece ->
          if Just field == (moveTo <$> hsLastMove game) then
            drawPiece piece # D.lc D.black # D.lw D.medium
          else
            drawPiece piece
        Nothing ->
          case M.lookup field labels of
            Just label -> drawLabel label
            Nothing -> mempty
    drawPiece :: Piece -> D.Diagram Backend
    drawPiece piece =
      let
        c = defaultTeamColours (pieceTeam piece)
        symbol = T.unpack (showPieceNumber (pieceNumber piece))
        text = D.text symbol # boardFontStyle # D.fc D.white
        circle = D.circle 0.3 # D.fc c
      in
        text `D.atop` circle
    drawLabel :: T.Text -> D.Diagram Backend
    drawLabel label =
      let
        text = D.text (T.unpack label) # boardFontStyle # D.fc D.gray
        circle = D.circle 0.3 # D.fc D.white
      in
        text `D.atop` circle

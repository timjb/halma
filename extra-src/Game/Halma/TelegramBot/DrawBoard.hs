{-# LANGUAGE FlexibleContexts #-}

module Game.Halma.TelegramBot.DrawBoard
  ( withRenderedBoardInPngFile
  ) where

import Game.Halma.Board
import Game.Halma.Board.Draw
import Game.Halma.TelegramBot.Move
import Game.Halma.TelegramBot.Types

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import Diagrams.Backend.Cairo (Cairo, renderCairo)
import Diagrams.Prelude ((#), (|||), (*^), (&), (.~))
import Diagrams.Query (resetValue)
import Diagrams.Size (dims)
import Diagrams.TwoD.Types (V2 (..))
import System.Directory (getTemporaryDirectory)
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import qualified Diagrams.Prelude as D
import qualified Data.Text as T

withRenderedBoardInPngFile
  :: (MonadIO m, MonadMask m)
  => HalmaState size
  -> (FilePath -> m a)
  -> m a
withRenderedBoardInPngFile game action =
  withTempPngFilePath $ \path -> do
    let
      dia = drawBoardForChat game # D.centerXY # D.pad 1.1
      bounds = dims (V2 1000 1000)
    liftIO $ renderCairo path bounds dia
    action path
  where
    withTempPngFilePath handler = do
      systemTempDir <- liftIO getTemporaryDirectory
      withTempFile systemTempDir "halma.png" $ \filePath fileHandle -> do
        liftIO (hClose fileHandle)
        handler filePath

drawBoardForChat :: HalmaState size -> D.Diagram Cairo
drawBoardForChat game =
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
    drawField :: (Int, Int) -> D.Diagram Cairo
    drawField field =
      case lookupHalmaBoard field board of
        Just piece ->
          if Just field == (moveTo <$> hsLastMove game) then
            drawPiece piece # D.lc D.black # D.lw D.medium
          else
            drawPiece piece
        Nothing ->
          mempty
    drawPiece :: Piece -> D.Diagram Cairo
    drawPiece piece =
      let
        c = defaultTeamColours (pieceTeam piece)
        symbol = T.unpack (showPieceNumber (pieceNumber piece))
        text = D.text symbol # boardFontStyle # D.fc D.white
        circle = D.circle 0.3 # D.fc c
      in
        text `D.atop` circle

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Game.Halma.TelegramBot.DrawBoard
  ( withRenderedBoardInPngFile
  ) where

import Game.Halma.Board
import Game.Halma.Board.Draw

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Colour.SRGB (sRGB24read)
import Diagrams.Backend.Cairo (Cairo, renderCairo)
import Diagrams.Prelude ((#), (|||), (*^), (&), (.~))
import Diagrams.Query (resetValue)
import Diagrams.Size (dims)
import Diagrams.TwoD.Types (V2 (..))
import Numeric (showHex)
import System.Directory (getTemporaryDirectory)
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import qualified Diagrams.Prelude as D

withRenderedBoardInPngFile
  :: (MonadIO m, MonadMask m)
  => HalmaBoard size
  -> (FilePath -> m a)
  -> m a
withRenderedBoardInPngFile board action =
  withTempPngFilePath $ \path -> do
    let
      dia = drawBoardForChat board # D.centerXY # D.pad 1.1
      bounds = dims (V2 1000 1000)
    liftIO $ renderCairo path bounds dia
    action path
  where
    withTempPngFilePath handler = do
      systemTempDir <- liftIO getTemporaryDirectory
      withTempFile systemTempDir "halma.png" $ \filePath fileHandle -> do
        liftIO (hClose fileHandle)
        handler filePath

drawBoardForChat :: HalmaBoard size -> D.Diagram Cairo
drawBoardForChat board =
  let
    boardDia = resetValue (drawBoard' (getGrid board) drawField)
  in
    scale ||| D.strut D.unitX ||| boardDia ||| D.strut D.unitX ||| scale
  where
    dirY = D.rotateBy (1/6) D.unitX
    rowDir = dirY & D._x .~ 0
    radiusInRows = 8 :: Int
    boardFontStyle x = x # D.fontSize (D.output 22) # D.font "Arial"
    scale = D.position $ map rowMarker [(-radiusInRows)..radiusInRows]
      where
        rowMarker i = (rowPosition i, rowNumber i)
        rowPosition i = D.p2 $ D.unr2 $ fromIntegral i *^ rowDir
        rowNumber j =
          let txt = show (j + radiusInRows)
          in D.text txt # boardFontStyle # D.fc D.gray
    -- colors from http://clrs.cc/
    botTeamColours :: Team -> D.Colour Double
    botTeamColours =
      \case
        North     -> sRGB24read "#0074D9" -- blue
        Northeast -> sRGB24read "#2ECC40" -- green
        Northwest -> sRGB24read "#B10DC9" -- purple
        South     -> sRGB24read "#FF4136" -- red
        Southeast -> sRGB24read "#111111" -- black
        Southwest -> sRGB24read "#FF851B" -- orange
    drawField :: (Int, Int) -> D.Diagram Cairo
    drawField field =
      maybe mempty drawPiece $ lookupHalmaBoard field board
    drawPiece :: Piece -> D.Diagram Cairo
    drawPiece piece =
      let
        c = botTeamColours (pieceTeam piece)
        symbol = showHex (pieceNumber piece) ""
        text = D.text symbol # boardFontStyle # D.fc D.white
        circle = D.circle 0.3 # D.fc c # D.lc (D.darken 0.5 c)
      in
        text `D.atop` circle

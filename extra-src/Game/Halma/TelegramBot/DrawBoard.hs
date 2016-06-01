{-# LANGUAGE LambdaCase #-}

module Game.Halma.TelegramBot.DrawBoard
  ( withRenderedBoardInPngFile
  ) where

import Game.Halma.Board
import Game.Halma.Board.Draw

import Control.Monad.Catch (MonadMask)
import Data.Colour.SRGB (sRGB24read)
import Diagrams.Backend.Cairo (Cairo, renderCairo)
import Diagrams.Prelude ((#))
import Diagrams.Query (resetValue)
import Diagrams.Size (dims)
import Diagrams.TwoD.Types (V2 (..))
import Control.Monad.IO.Class (MonadIO (..))
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
      dia = drawBoardForChat board
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
  resetValue (drawBoard' (getGrid board) drawField)
  where
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
    drawPiece
      :: Piece -> D.Diagram Cairo
    drawPiece piece =
      let
        c = botTeamColours (pieceTeam piece)
        symbol =
          case pieceNumber piece of
            1 -> '1'
            2 -> '2'
            3 -> '3'
            4 -> '4'
            5 -> '5'
            6 -> '6'
            7 -> '7'
            8 -> '8'
            9 -> '9'
            10 -> 'a'
            11 -> 'b'
            12 -> 'c'
            13 -> 'd'
            14 -> 'e'
            15 -> 'f'
            i -> error ("unexpected piece number: " ++ show i)
        text =
          D.text [symbol] # D.fc D.white #
          D.fontSize (D.output 20) # D.font "Arial"
        circle = D.circle 0.25 # D.fc c # D.lc (D.darken 0.5 c)
      in
        text `D.atop` circle
    drawField :: (Int, Int) -> D.Diagram Cairo
    drawField field =
      maybe mempty drawPiece $ lookupHalmaBoard field board
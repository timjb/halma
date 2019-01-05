{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Game.Halma.TelegramBot.CmdLineOptions
import Game.Halma.TelegramBot.Controller (halmaBot)
import Game.Halma.TelegramBot.Controller.BotM (evalGlobalBotM)
import Game.Halma.TelegramBot.Controller.Types (BotConfig (..))
import Game.Halma.TelegramBot.Controller.Persistence (noPersistence, filePersistence)

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import qualified Options.Applicative as OA

main :: IO ()
main = do
  opts <- OA.execParser optionsParserInfo
  manager <- newManager tlsManagerSettings
  let
    persistence =
      case boOutputDirectory opts of
        Nothing -> noPersistence
        Just outDir -> filePersistence outDir
    cfg =
      BotConfig
        { bcToken = boToken opts
        , bcPersistence = persistence
        , bcManager = manager
        }
  print opts
  evalGlobalBotM halmaBot cfg >>= \case
    Left e -> do
      hPrint stderr e
      exitFailure
    Right () -> return ()


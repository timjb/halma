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

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Options.Applicative as OA

main :: IO ()
main = do
  opts <- OA.execParser optionsParserInfo
  manager <- newManager tlsManagerSettings
  let
    cfg =  
      BotConfig
        { bcToken = boToken opts
        , bcOutputDirectory = boOutputDirectory opts
        , bcManager = manager
        }
  print opts
  evalGlobalBotM halmaBot cfg


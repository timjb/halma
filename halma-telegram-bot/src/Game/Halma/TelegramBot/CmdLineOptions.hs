{-# LANGUAGE OverloadedStrings #-}

module Game.Halma.TelegramBot.CmdLineOptions
  ( BotOptions (..)
  , optionsParserInfo
  ) where

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Options.Applicative as OA
import qualified Web.Telegram.API.Bot as TG

data BotOptions
  = BotOptions
  { boToken :: TG.Token
  , boOutputDirectory :: Maybe FilePath
  } deriving (Show, Eq)

optionsParserInfo :: OA.ParserInfo BotOptions
optionsParserInfo =
  OA.info (OA.helper <*> optionsParser) $ mconcat
    [ OA.header "halma-telegram-bot -- Play Chinese Checkers in Telegram Chats"
    , OA.progDesc "This is the server for the Telegram HalmaBot (https://github.com/timjb/halma)"
    ]

optionsParser :: OA.Parser BotOptions
optionsParser =
  BotOptions
    <$> tokenP
    <*> saveDirectoryP
  where
    tokenP =
      fmap (TG.Token . ("bot" `ensureIsPrefixOf`) . T.pack) $
      OA.strArgument $ mconcat
        [ OA.metavar "TELEGRAM_TOKEN"
        ]
    saveDirectoryP =
      OA.optional $ OA.strOption $ mconcat
        [ OA.long "output"
        , OA.short 'o'
        , OA.metavar "OUTPUT_DIR"
        , OA.help "Where to save chat data"
        ]

ensureIsPrefixOf :: T.Text -> T.Text -> T.Text
ensureIsPrefixOf prefix str =
  if prefix `T.isPrefixOf` str then
    str
  else
    prefix <> str


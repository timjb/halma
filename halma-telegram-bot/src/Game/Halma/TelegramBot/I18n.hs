{-# LANGUAGE OverloadedStrings #-}

module Game.Halma.TelegramBot.I18n
  ( HalmaLocale (..)
  , enHalmaLocale
  , deHalmaLocale
  , LocaleId (..)
  , allLocaleIds
  , showLocaleId
  , parseLocaleId
  , localeById
  ) where

import Data.Monoid ((<>))
import qualified Data.Aeson as A
import qualified Data.Text as T

data HalmaLocale
  = HalmaLocale
  { hlWelcomeMsg :: T.Text
  , hlHelpMsg :: T.Text
  } deriving (Show)

-- | English
enHalmaLocale :: HalmaLocale
enHalmaLocale =
  HalmaLocale
    { hlWelcomeMsg =
        "Greetings from HalmaBot! I am an [open-source](https://github.com/timjb/halma) Telegram bot " <>
        "written in Haskell by Tim Baumann <tim@timbaumann.info>."
    , hlHelpMsg =
        "You can control me by sending these commands:\n" <>
        "/newmatch — starts a new match between two or three players\n" <>
        "/newround — start a new game round\n" <>
        "/undo — reverse your last move\n" <>
        "/setlang [" <> localesText <> "] — switch to another language\n" <>
        "/help — display this message\n\n" <>
        "Here's how move commands are structured:\n" <>
        "First there comes a letter in the range A-O (the piece you want to " <>
        " move), then the number of the row you want to move the piece to. " <>
        "If there are multiple possible target positions on the row, I will " <>
        "ask you which one you mean.\n" <>
        "For example: the command 'a11' makes me move your piece labeled 'a' " <>
        "to row number 11."
    }

-- | German (deutsch)
deHalmaLocale :: HalmaLocale
deHalmaLocale =
  HalmaLocale
    { hlWelcomeMsg =
        "Hallo, ich bin @HalmaBot, ein [quelloffener](https://github.com/timjb/halma) Telegram-Bot " <>
        " programmiert von Tim Baumann <tim@timbaumann.info> in Haskell."
    , hlHelpMsg =
        "Du kannst mich durch folgende Kommandos steuern:\n" <>
        "/newmatch — startet ein neues Halma-Match\n" <>
        "/newround — startet eine neue Spielrunde in einem Match\n" <>
        "/undo — macht den letzten Zug rückgängig\n" <>
        "/setlang [" <> localesText <> "] — wechsle die Sprache / switch to another language\n" <>
        "/help — zeigt diese Hilfe-Nachricht an\n\n" <>
        "Zuganweisungen sind folgendermaßen aufgebaut:\n" <>
        "Zuerst kommt ein Buchstabe von A bis O (der Spielstein, den du " <>
        " bewegen möchtest), dann kommt die Nummer der Reihe in den du den " <>
        " Stein bewegen möchtest. " <>
        "Wenn es mehrere mögliche Zielpositionen innerhalb dieser Reihe gibt, " <>
        "dann frage ich dich, welche du genau meinst.\n" <>
        "Zum Beispiel: Das Kommando 'a11' bewegt deinen Stein mit der " <>
        "Aufschrift 'a' in die Zeile Nummer 11."
    }

data LocaleId
  = En
  | De
  deriving (Show, Eq)

instance A.ToJSON LocaleId where
  toJSON = A.String . showLocaleId

instance A.FromJSON LocaleId where
  parseJSON =
    A.withText "LocaleId" $ \t ->
      case parseLocaleId t of
        Nothing -> fail "unrecognized locale id"
        Just localeId -> pure localeId

allLocaleIds :: [LocaleId]
allLocaleIds = [En, De]

showLocaleId :: LocaleId -> T.Text
showLocaleId localeId =
  case localeId of
    En -> "en"
    De -> "de"

localesText :: T.Text
localesText = T.intercalate "/" (map showLocaleId allLocaleIds)

parseLocaleId :: T.Text -> Maybe LocaleId
parseLocaleId text =
  case T.toLower text of
    "de" -> Just De
    "en" -> Just En
    _ -> Nothing

localeById :: LocaleId -> HalmaLocale
localeById localeId =
  case localeId of
    En -> enHalmaLocale
    De -> deHalmaLocale
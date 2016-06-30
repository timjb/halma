{-# LANGUAGE OverloadedStrings #-}

module Game.Halma.TelegramBot.View.I18n
  ( HalmaLocale (..)
  , enHalmaLocale
  , deHalmaLocale
  , LocaleId (..)
  , allLocaleIds
  , localeById
  ) where
    
import Game.Halma.TelegramBot.Model.Types
import Game.Halma.TelegramBot.View.Pretty

import Data.Char (toUpper)
import Data.Monoid ((<>))
import qualified Data.Aeson as A
import qualified Data.Text as T

data HalmaLocale
  = HalmaLocale
  { hlWelcomeMsg :: T.Text
  , hlHelpMsg :: T.Text
  , hlCongratulation :: ExtendedPartyResult -> T.Text
  }

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
    , hlCongratulation = congrat
    }
  where
    nominal i =
      case i of
        1 -> "first"
        2 -> "second"
        3 -> "third"
        4 -> "fourth"
        5 -> "fifth"
        6 -> "sixth"
    deficitMsg result =
      "with a deficit of " <> T.pack (show (eprLag result)) <> " moves"
    congrat result =
      let
        party = prParty $ eprPartyResult result
      in
      case (partyPlayer party, eprPlace result) of
        (AIPlayer, i) ->
          "The AI " <> teamEmoji (partyHomeCorner party) <> " finishes " <>
          nominal (i+1) <>
          (if i == 0 then "" else " " <> deficitMsg result)
        (TelegramPlayer user, 0) ->
          prettyUser user <> ", " <>
          "congratulations on your " <>
          (if eprPlaceShared result then "shared " else "") <>
          "first place \127941" -- unicode symbol: sports medal
        (TelegramPlayer user, i) ->
          prettyUser user <> ", " <>
          "you are " <> nominal (i+1) <>
          " " <> deficitMsg result

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
    , hlCongratulation = congrat
    }
  where
    nominal i =
      case i of
        1 -> "erster"
        2 -> "zweiter"
        3 -> "dritter"
        4 -> "vierter"
        5 -> "fünfter"
        6 -> "sechster"
    deficitMsg result =
      "mit einem Rückstand von " <>
      T.pack (show (eprLag result)) <>
      " Zügen"
    congrat result =
      let
        party = prParty $ eprPartyResult result
      in
      case (partyPlayer party, eprPlace result) of
        (AIPlayer, i) ->
          "Die KI " <> teamEmoji (partyHomeCorner party) <> " wird " <>
          (if eprPlaceShared result then "ebenfalls " else "") <>
          capitalize (nominal (i+1)) <>
          (if i == 0 then "" else " " <> deficitMsg result)
        (TelegramPlayer user, 0) ->
          prettyUser user <> ", " <>
          "Glückwunsch zum " <>
          (if eprPlaceShared result then "geteilten " else "") <>
          "ersten Platz \127941" -- unicode symbol: sports medal
        (TelegramPlayer user, i) ->
          prettyUser user <> ", " <>
          "du bist " <>
          (if eprPlaceShared result then "auch " else "") <>
          capitalize (nominal (i+1)) <>
          " " <> deficitMsg result

capitalize :: T.Text -> T.Text
capitalize t =
  if T.null t then
    t
  else
    toUpper (T.head t) `T.cons` T.tail t

allLocaleIds :: [LocaleId]
allLocaleIds = [En, De]

localesText :: T.Text
localesText = T.intercalate " / " (map prettyLocaleId allLocaleIds)

localeById :: LocaleId -> HalmaLocale
localeById localeId =
  case localeId of
    En -> enHalmaLocale
    De -> deHalmaLocale
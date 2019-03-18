{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Halma.TelegramBot.View.I18n
  ( HalmaLocale (..)
  , NotYourTurnInfo (..)
  , CantUndoReason (..)
  , AIMove (..)
  , enHalmaLocale
  , deHalmaLocale
  , LocaleId (..)
  , allLocaleIds
  , localeById
  ) where

import Game.Halma.Board (HalmaDirection)
import Game.Halma.Configuration
import Game.Halma.TelegramBot.Controller.SlashCmd
import Game.Halma.TelegramBot.Model.MoveCmd
import Game.Halma.TelegramBot.Model.Types
import Game.Halma.TelegramBot.View.Pretty

import Data.Char (toUpper)
import Data.Foldable (toList)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as TG

data NotYourTurnInfo
  = NotYourTurnInfo
  { you :: TG.User
  , thePlayerWhoseTurnItIs :: Player
  }

data CantUndoReason
  = CantUndoNoGame

data AIMove
  = AIMove
  { aiHomeCorner :: HalmaDirection
  , aiMoveCmd :: MoveCmd
  }

data HalmaLocale
  = HalmaLocale
  { hlNewMatchCmd :: T.Text
  , hlNewRoundCmd :: T.Text
  , hlUndoCmd :: T.Text
  , hlHelpCmd :: T.Text
  , hlWelcomeMsg :: T.Text
  , hlHelpMsg :: T.Text
  , hlCongratulation :: ExtendedPartyResult -> T.Text
  , hlCantStartNewRoundBecauseNoMatch :: T.Text
  , hlStartingANewRound :: T.Text
  , hlYourTurn :: HalmaDirection -> TG.User -> T.Text
  , hlNotYourTurn :: NotYourTurnInfo -> T.Text
  , hlCantUndo :: Maybe CantUndoReason -> T.Text
  , hlAIMove :: AIMove -> T.Text
  , hlNoMatchMsg :: T.Text
  , hlNoRoundMsg :: T.Text
  , hlAmbiguousMoveCmdMsg :: TG.User -> MoveCmd -> T.Text
  , hlImpossibleMoveCmdMsg :: TG.User -> MoveCmd -> T.Text
  , hlUnrecognizedCmdMsg :: CmdCall -> T.Text
  , hlGatheringPlayersMsg :: PlayersSoFar Player -> T.Text
  , hlMe :: T.Text
  , hlAnAI :: T.Text
  , hlYesMe :: T.Text
  , hlYesAnAI :: T.Text
  , hlNo :: T.Text
  }

-- | English
enHalmaLocale :: HalmaLocale
enHalmaLocale =
  HalmaLocale
    { hlNewMatchCmd = newMatchCmd
    , hlNewRoundCmd = newRoundCmd
    , hlUndoCmd = undoCmd
    , hlHelpCmd = helpCmd
    , hlWelcomeMsg =
        "Greetings from HalmaBot! I am an [open-source](https://github.com/timjb/halma) Telegram bot " <>
        "written in Haskell by Tim Baumann <tim@timbaumann.info>."
    , hlHelpMsg =
        "You can control me by sending these commands:\n" <>
        "/" <> newMatchCmd <> " — starts a new match between two or three players\n" <>
        "/" <> newRoundCmd <> " — start a new game round\n" <>
        "/" <> undoCmd <> " — reverse your last move\n" <>
        "/deutsch — " <> localeFlag De <> " auf Deutsch umschalten / switch to German\n" <>
        "/" <> helpCmd <> " — display this message\n\n" <>
        "Here's how move commands are structured:\n" <>
        "First there comes a letter in the range A-O (the piece you want to " <>
        " move), then the number of the row you want to move the piece to. " <>
        "If there are multiple possible target positions on the row, I will " <>
        "ask you which one you mean.\n" <>
        "For example: the command 'A11' makes me move your piece labeled 'A' " <>
        "to row number 11."
    , hlCongratulation = congrat
    , hlCantStartNewRoundBecauseNoMatch =
        "Can't start a new round, because there is no match running. You have to start a /newmatch first."
    , hlStartingANewRound =
        "starting a new round!"
    , hlYourTurn = \homeCorner user ->
        prettyUser user <> " " <> teamEmoji homeCorner <> " it's your turn!"
    , hlNotYourTurn = \notYourTurn ->
        "Hey " <> prettyUser (you notYourTurn) <> ", it's not your turn, it's " <>
        prettyPlayer (thePlayerWhoseTurnItIs notYourTurn) <> "'s!"
    , hlCantUndo = \case
        Nothing -> "can't undo!"
        Just CantUndoNoGame -> "can't undo: no game running!"
    , hlAIMove = \aiMove ->
        "The AI " <> teamEmoji (aiHomeCorner aiMove) <>
        " makes the following move: " <> showMoveCmd (aiMoveCmd aiMove)
    , hlNoMatchMsg =
        "Start a new Halma match with /" <> newMatchCmd
    , hlNoRoundMsg =
        "Start a new round with /" <> newRoundCmd
    , hlAmbiguousMoveCmdMsg = \sender _moveCmd ->
        prettyUser sender <> ", the move command you sent is ambiguous. " <>
        "Please send another move command or choose one in the " <>
        "following list."
    , hlImpossibleMoveCmdMsg = \_sender moveCmd ->
        "The selected piece can't be moved to row " <>
        T.pack (show (unRowNumber (moveTargetRow moveCmd))) <> "!"
    , hlUnrecognizedCmdMsg = \(CmdCall { cmdCallName = cmd }) ->
        "Unknown command /" <> cmd <> ". See /" <> helpCmd <> " for a list of all commands."
    , hlGatheringPlayersMsg = gatheringPlayersMsg
    , hlMe = "me"
    , hlAnAI = "an AI"
    , hlYesMe = "yes, me"
    , hlYesAnAI = "yes, an AI"
    , hlNo = "no"
    }
  where
    newMatchCmd, newRoundCmd, undoCmd, helpCmd :: T.Text
    newMatchCmd = "newmatch"
    newRoundCmd = "newround"
    undoCmd = "undo"
    helpCmd = "help"
    nominal i =
      case i of
        1 -> "first"
        2 -> "second"
        3 -> "third"
        4 -> "fourth"
        5 -> "fifth"
        6 -> "sixth"
        _ -> "(error: unexpected integer)"
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
    prettyPlayer :: Player -> T.Text
    prettyPlayer player =
      case player of
        AIPlayer -> "AI"
        TelegramPlayer user -> prettyUser user
    prettyList :: [T.Text] -> T.Text
    prettyList xs =
      case xs of
        [] -> "<empty list>"
        [x] -> x
        _:_:_ -> T.intercalate ", " (init xs) <> " and " <> last xs
    gatheringPlayersMsg = \case
      NoPlayers -> "Starting a new match! Who is the first player?"
      OnePlayer firstPlayer ->
        "The first player is " <> prettyPlayer firstPlayer <> ".\n" <>
        "Who is the second player?"
      EnoughPlayers config ->
        let
          (count, nextOrdinal) = case configurationPlayers config of
            TwoPlayers {}   -> ("two", "third")
            ThreePlayers {} -> ("three", "fourth")
            FourPlayers {}  -> ("four", "fifth")
            FivePlayers {}  -> ("five", "sixth")
            SixPlayers {}   -> error "unexpected state: gathering players although there are already six!"
        in
          "The first " <> count <> " players are " <>
          prettyList (map prettyPlayer (toList (configurationPlayers config))) <> ".\n" <>
          "Is there a " <> nextOrdinal <> " player?"

-- | German (deutsch)
deHalmaLocale :: HalmaLocale
deHalmaLocale =
  HalmaLocale
    { hlNewMatchCmd = newMatchCmd
    , hlNewRoundCmd = newRoundCmd
    , hlUndoCmd = undoCmd
    , hlHelpCmd = helpCmd
    , hlWelcomeMsg =
        "Hallo, ich bin @HalmaBot, ein [quelloffener](https://github.com/timjb/halma) Telegram-Bot " <>
        " programmiert von Tim Baumann <tim@timbaumann.info> in Haskell."
    , hlHelpMsg =
        "Du kannst mich durch folgende Kommandos steuern:\n" <>
        "/" <> newMatchCmd <> " — startet einen neuen Halma-Wettkampf\n" <>
        "/" <> newRoundCmd <> " — startet eine neue Spielrunde im aktuellen Wettkampf\n" <>
        "/" <> undoCmd <> " — macht den letzten Zug rückgängig\n" <>
        "/english — " <> localeFlag En <> " switch to English / auf Englisch umschalten\n" <>
        "/" <> helpCmd <> " — zeigt diese Hilfe-Nachricht an\n\n" <>
        "Zuganweisungen sind folgendermaßen aufgebaut:\n" <>
        "Zuerst kommt ein Buchstabe von A bis O (der Spielstein, den du " <>
        " bewegen möchtest), dann kommt die Nummer der Reihe in den du den " <>
        " Stein bewegen möchtest. " <>
        "Wenn es mehrere mögliche Zielpositionen innerhalb dieser Reihe gibt, " <>
        "dann frage ich dich, welche du genau meinst.\n" <>
        "Zum Beispiel: Das Kommando 'A11' bewegt deinen Stein mit der " <>
        "Aufschrift 'A' in die Zeile Nummer 11."
    , hlCongratulation = congrat
    , hlCantStartNewRoundBecauseNoMatch =
        "Um eine neue Runde zu starten, muss erst ein neues Spiel mit /newmatch gestartet werden."
    , hlStartingANewRound =
        "Neue Runde!"
    , hlYourTurn = \homeCorner user ->
        prettyUser user <> " " <> teamEmoji homeCorner <> ", du bist dran!"
    , hlNotYourTurn = \notYourTurn ->
        "Hey " <> prettyUser (you notYourTurn) <> ", du bist nicht an der Reihe, sondern " <>
        prettyPlayer (thePlayerWhoseTurnItIs notYourTurn) <> "!"
    , hlCantUndo = \case
        Nothing -> "'undo' nicht möglich!"
        Just CantUndoNoGame -> "'undo' nicht möglich: es ist gerade kein Spiel am Laufen!"
    , hlAIMove = \aiMove ->
        "Die KI " <> teamEmoji (aiHomeCorner aiMove) <>
        " macht den folgenden Zug: " <> showMoveCmd (aiMoveCmd aiMove)
    , hlNoMatchMsg =
        "Starte einen neuen Halma-Wettkampf mit /" <> newMatchCmd
    , hlNoRoundMsg =
        "Starte eine neue Runde mit /" <> newRoundCmd
    , hlAmbiguousMoveCmdMsg = \sender _moveCmd ->
        prettyUser sender <> ", deine Zuganweisung ist nicht eindeutig. " <>
        "Sende bitte eine andere Zuganweisung oder präzisiere deine Anweisung mit Hilfe " <>
        "der folgenden Liste."
    , hlImpossibleMoveCmdMsg = \_sender moveCmd ->
        "Der Spielstein '" <> showPieceNumber (movePieceNumber moveCmd) <> "' kann nicht in " <>
        "die Reihe " <> T.pack (show (unRowNumber (moveTargetRow moveCmd))) <> " bewegt werden!"
    , hlUnrecognizedCmdMsg = \(CmdCall { cmdCallName = cmd }) ->
        "Unbekanntes Kommando /" <> cmd <> ". Eine Liste aller Kommandos liefert /" <> helpCmd <> "."
    , hlGatheringPlayersMsg = gatheringPlayersMsg
    , hlMe = "ich"
    , hlAnAI = "eine KI"
    , hlYesMe = "ja, ich"
    , hlYesAnAI = "ja, eine KI"
    , hlNo = "nein"
    }
  where
    newMatchCmd, newRoundCmd, undoCmd, helpCmd :: T.Text
    newMatchCmd = "neuerwettkampf"
    newRoundCmd = "neuerunde"
    undoCmd = "zurueck"
    helpCmd = "hilfe"
    nominal i =
      case i of
        1 -> "erster"
        2 -> "zweiter"
        3 -> "dritter"
        4 -> "vierter"
        5 -> "fünfter"
        6 -> "sechster"
        _ -> "(Fehler: unerwartete Zahl)"
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
    prettyPlayer :: Player -> T.Text
    prettyPlayer player =
      case player of
        AIPlayer -> "eine KI"
        TelegramPlayer user -> prettyUser user
    prettyList :: [T.Text] -> T.Text
    prettyList xs =
      case xs of
        [] -> "<leere Liste>"
        [x] -> x
        _:_:_ -> T.intercalate ", " (init xs) <> " und " <> last xs
    gatheringPlayersMsg = \case
      NoPlayers -> "Ein neuer Wettkampf! Wer macht mit?"
      OnePlayer firstPlayer ->
        prettyPlayer firstPlayer <> " fängt also an.\n" <>
        "Wer macht noch mit?"
      EnoughPlayers config ->
        let
          count = case configurationPlayers config of
            TwoPlayers {}   -> "zwei"
            ThreePlayers {} -> "drei"
            FourPlayers {}  -> "vier"
            FivePlayers {}  -> "fünf"
            SixPlayers {}   -> "secht"
        in
          "Die ersten " <> count <> " Parteien sind " <>
          prettyList (map prettyPlayer (toList (configurationPlayers config))) <> ".\n" <>
          "Macht noch jemand mit?"

capitalize :: T.Text -> T.Text
capitalize t =
  if T.null t then
    t
  else
    toUpper (T.head t) `T.cons` T.tail t

allLocaleIds :: [LocaleId]
allLocaleIds = [En, De]

localeById :: LocaleId -> HalmaLocale
localeById localeId =
  case localeId of
    En -> enHalmaLocale
    De -> deHalmaLocale

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Game.Halma.TelegramBot.Controller
  ( handleUpdate
  , halmaBot
  ) where

import Game.Halma.Board
import Game.Halma.Configuration
import Game.Halma.TelegramBot.Controller.BotM
import Game.Halma.TelegramBot.Controller.Helpers
import Game.Halma.TelegramBot.Controller.SlashCmd
import Game.Halma.TelegramBot.Controller.Types
import Game.Halma.TelegramBot.Model
import Game.Halma.TelegramBot.View
import Game.TurnCounter
import qualified Game.Halma.AI.Ignorant as IgnorantAI
import qualified Game.Halma.AI.Competitive as CompetitiveAI

import Control.Concurrent (threadDelay)
import Control.Monad (void, when)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (MonadState (..), gets, modify)
import System.IO (hPrint, stderr)
import System.TimeIt (timeItNamed)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as TG

getUpdates :: GlobalBotM [TG.Update]
getUpdates = do
  nid <- gets bsNextId
  let
    limit = 100
    timeout = 10
    updateReq token =
      TG.getUpdates token (Just nid) (Just limit) (Just timeout)
  TG.Response updates _resParams <- runReq updateReq
  unless (null updates) $ do
    let nid' = 1 + maximum (map TG.update_id updates)
    modify (\s -> s { bsNextId = nid' })
  return updates

getUpdatesRetry :: GlobalBotM [TG.Update]
getUpdatesRetry =
  getUpdates `botCatch` \err -> do
    liftIO $ hPrint stderr err
    liftIO $ threadDelay 1000000 -- wait one second
    getUpdatesRetry

sendCurrentBoard :: HalmaState -> BotM ()
sendCurrentBoard halmaState =
  withRenderedBoardInPngFile halmaState mempty $ \path -> do
    chatId <- gets hcId
    let
      fileUpload = TG.localFileUpload path
      photoReq = TG.uploadPhotoRequest (TG.ChatId chatId) fileUpload
    timeItNamed "Uploading and sending board image (CPU time)" $
      void $ runReq $ \token -> TG.uploadPhoto token photoReq

handleCommand :: CmdCall -> BotM ()
handleCommand call@(CmdCall { cmdCallName = cmd }) = do
  locale <- getLocale
  let
    isCmd f = f locale == cmd
  if isCmd hlHelpCmd then
    sendI18nMsg hlHelpMsg
  else if cmd == "start" then do
    modify $ \chat ->
      chat { hcMatchState = NoMatch }
    sendI18nMsg hlWelcomeMsg
    sendMatchState
  else if cmd == "english" then do
    modify $ \chat -> chat { hcLocale = En }
    sendMatchState
  else if cmd == "deutsch" then do
    modify $ \chat -> chat { hcLocale = De }
    sendMatchState
  else if isCmd hlNewMatchCmd then do
    modify $ \chat ->
      chat { hcMatchState = GatheringPlayers NoPlayers }
    sendMatchState
  else if isCmd hlNewRoundCmd then do
    matchState <- gets hcMatchState
    case matchState of
      MatchRunning match -> do
        sendI18nMsg hlStartingANewRound
        modify $ \chat ->
          chat { hcMatchState = MatchRunning (newRound match) }
        sendMatchState
      _ -> do
        sendI18nMsg hlCantStartNewRoundBecauseNoMatch
  else if isCmd hlUndoCmd then do
    matchState <- gets hcMatchState
    case matchState of
      MatchRunning match@Match{ matchCurrentGame = Just game } ->
        case undoLastMove game of
          Just game' -> do
            let
              match' = match { matchCurrentGame = Just game' }
            modify $ \chat ->
              chat { hcMatchState = MatchRunning match' }
            sendMatchState
          Nothing -> do
            sendI18nMsg (flip hlCantUndo Nothing)
      _ -> do
        sendI18nMsg (flip hlCantUndo (Just CantUndoNoGame))
  else
    sendI18nMsg (flip hlUnrecognizedCmdMsg call)

sendMoveSuggestions
  :: TG.User
  -> TG.Message
  -> HalmaState
  -> NonEmpty (MoveCmd, Move)
  -> BotM ()
sendMoveSuggestions sender msg game suggestions = do
  let
    text =
      prettyUser sender <> ", the move command you sent is ambiguous. " <>
      "Please send another move command or choose one in the " <>
      "following list."
    suggestionToButton (moveCmd, _move) =
      showMoveCmd moveCmd
    keyboard =
      (mkKeyboard [suggestionToButton <$> toList suggestions])
      { TG.reply_selective = Just True
      }
    suggestionToLabel (moveCmd, move) =
      ( moveTo move
      , maybe "" showTargetModifier (moveTargetModifier moveCmd)
      )
    boardLabels =
      M.fromList $ map suggestionToLabel $ toList suggestions
  withRenderedBoardInPngFile game boardLabels $ \path -> do
    chatId <- gets hcId
    let
      fileUpload = TG.localFileUpload path
      photoReq =
        (TG.uploadPhotoRequest (TG.ChatId chatId) fileUpload)
          { TG.photo_caption = Just text
          , TG.photo_reply_to_message_id = Just (TG.message_id msg)
          , TG.photo_reply_markup = Just keyboard
          }
    void $ runReq $ \token -> TG.uploadPhoto token photoReq

announceResult
  :: ExtendedPartyResult
  -> BotM ()
announceResult extendedResult =
  sendI18nMsg $ \locale -> hlCongratulation locale extendedResult

handleMoveCmd
  :: Match
  -> HalmaState
  -> MoveCmd
  -> TG.Message
  -> BotM ()
handleMoveCmd match game moveCmd fullMsg = do
  logMsg $ "Received move command: " ++ T.unpack (showMoveCmd moveCmd)
  case TG.from fullMsg of
    Nothing -> do
      sendMsg $ textMsg $
        "Can't identify sender of move command " <> showMoveCmd moveCmd <> "!"
    Just sender -> do
      let
        currParty = currentPlayer (hsTurnCounter game)
        homeCorner = partyHomeCorner currParty
        player = partyPlayer currParty
        checkResult =
          checkMoveCmd (matchRules match) (hsBoard game) homeCorner moveCmd
      case checkResult of
        _ | player /= TelegramPlayer sender -> do
          let notYourTurnInfo =
                NotYourTurnInfo
                { you = sender
                , thePlayerWhoseTurnItIs = player
                }
          sendI18nMsg (flip hlNotYourTurn notYourTurnInfo)
        MoveImpossible reason -> do
          logMsg $ "Move is not possible: " <> reason
          sendMsg $ textMsg $
            "This move is not possible: " <> T.pack reason
        MoveSuggestions suggestions -> do
          logMsg "Command does not describe a unique move. Sending suggestions ..."
          sendMoveSuggestions sender fullMsg game suggestions
        MoveFoundUnique move -> do
          logMsg "Move is valid"
          case doMove move game of
            Left err ->
              botThrow err
            Right afterMove -> do
              handleAfterMove match game afterMove

handleAfterMove
  :: Match
  -> HalmaState
  -> AfterMove
  -> BotM ()
handleAfterMove match game afterMove =
  case afterMove of
    GameEnded lastResult -> do
      announceResult lastResult
      let
        gameResult =
          GameResult
            { grNumberOfMoves = hsFinished game ++ [eprPartyResult lastResult]
            }
        match' =
          match
            { matchCurrentGame = Nothing
            , matchHistory = matchHistory match ++ [gameResult]
            }
      modify $ \chat ->
        chat { hcMatchState = MatchRunning match' }
      sendMatchState
      -- TODO: offer to start new game
    GameContinues mPartyResult game' -> do
      mapM_ announceResult mPartyResult
      let
        match' = match { matchCurrentGame = Just game' }
      modify $ \chat ->
        chat { hcMatchState = MatchRunning match' }
      sendMatchState

handleTextMsg
  :: T.Text
  -> TG.Message
  -> BotM ()
handleTextMsg text fullMsg = do
  matchState <- gets hcMatchState
  locale <- getLocale
  case (matchState, text) of
    (_, parseCmdCall -> Just cmdCall) ->
      handleCommand cmdCall
    (MatchRunning match@Match { matchCurrentGame = Just game }, parseMoveCmd -> Right moveCmd) ->
      handleMoveCmd match game moveCmd fullMsg
    (GatheringPlayers players, (`elem` [hlMe locale, hlYesMe locale]) -> True) -> do
      addTelegramPlayer players
      sendMatchState
    (GatheringPlayers players, (`elem` [hlAnAI locale, hlYesAnAI locale]) -> True) -> do
      addAIPlayer players
      sendMatchState
    (GatheringPlayers (EnoughPlayers config), _) | text == hlNo locale -> do
      startMatch config
      sendMatchState
    _ -> pure ()
  where
    addPlayer :: Player -> PlayersSoFar Player -> BotM ()
    addPlayer new playersSoFar = do
      playersSoFar' <-
        case playersSoFar of
          NoPlayers ->
            pure $ OnePlayer new
          OnePlayer a ->
            case configuration SmallGrid (TwoPlayers a new) of
              Just config -> pure $ EnoughPlayers config
              Nothing -> fail "could not create configuration with two players and small grid!"
          EnoughPlayers config ->
            pure $ EnoughPlayers (addPlayerToConfig new config)
      chat <- get
      case playersSoFar' of
        EnoughPlayers config@(configurationPlayers -> SixPlayers{}) ->
          put $ chat { hcMatchState = MatchRunning (newMatch config) }
        _ ->
          put $ chat { hcMatchState = GatheringPlayers playersSoFar' }
    addAIPlayer :: PlayersSoFar Player -> BotM ()
    addAIPlayer = addPlayer AIPlayer
    addTelegramPlayer :: PlayersSoFar Player -> BotM ()
    addTelegramPlayer players =
      case TG.from fullMsg of
        Nothing ->
          sendMsg $ textMsg "cannot add sender of last message as a player!"
        Just user ->
          addPlayer (TelegramPlayer user) players
    startMatch players =
      modify $ \chat ->
        chat { hcMatchState = MatchRunning (newMatch players) }

sendGatheringPlayers :: PlayersSoFar Player -> BotM ()
sendGatheringPlayers playersSoFar = do
  locale <- getLocale
  let
    meKeyboard = mkKeyboard [[hlMe locale], [hlAnAI locale]]
    gatherMsg = hlGatheringPlayersMsg locale playersSoFar
  case playersSoFar of
    NoPlayers ->
      sendMsg $ textMsgWithKeyboard gatherMsg meKeyboard
    OnePlayer _ ->
      sendMsg $ textMsgWithKeyboard gatherMsg meKeyboard
    EnoughPlayers _ -> do
      let
        anotherPlayerKeyboard =
          mkKeyboard [[hlYesMe locale], [hlYesAnAI locale], [hlNo locale]]
      sendMsg (textMsgWithKeyboard gatherMsg anotherPlayerKeyboard)

doAIMove :: Match -> HalmaState -> BotM ()
doAIMove match game = do
  let
    tc = hsTurnCounter game
    board = hsBoard game
    rules = matchRules match
    numberOfPlayers = length $ tcPlayers tc
    currParty = currentPlayer tc
    aiMove =
      if numberOfPlayers == 2 then
        let
          nextParty = nextPlayer tc
          perspective = (partyHomeCorner currParty, partyHomeCorner nextParty)
        in
          CompetitiveAI.aiMove rules board perspective
      else
        IgnorantAI.aiMove rules board (partyHomeCorner currParty)
    mAIMoveCmd = moveToMoveCmd rules board aiMove
  afterMove <- timeItNamed "Calculating AI move (CPU time)" $
    case doMove aiMove game of
      Left err ->
        fail $ "doing an AI move failed unexpectedly: " ++ err
      Right afterMove -> pure afterMove
  case mAIMoveCmd of
    Just moveCmd ->
      let moveByAi =
            AIMove
            { aiHomeCorner = partyHomeCorner currParty
            , aiMoveCmd = moveCmd
            }
      in sendI18nMsg (flip hlAIMove moveByAi)
    Nothing -> pure ()
  handleAfterMove match game afterMove

sendGameState :: Match -> HalmaState -> BotM ()
sendGameState match game = do
  sendCurrentBoard game
  let
    currParty = currentPlayer (hsTurnCounter game)
  case partyPlayer currParty of
    AIPlayer -> doAIMove match game
    TelegramPlayer user ->
      sendI18nMsg (\hl -> hlYourTurn hl (partyHomeCorner currParty) user)

sendMatchState :: BotM ()
sendMatchState = do
  logMsg "Sending match state ..."
  matchState <- gets hcMatchState
  case matchState of
    NoMatch ->
      sendI18nMsg hlNoMatchMsg
    GatheringPlayers players ->
      sendGatheringPlayers players
    MatchRunning match ->
      case matchCurrentGame match of
        Nothing ->
          sendI18nMsg hlNoRoundMsg
        Just game -> sendGameState match game

loadHalmaChat :: ChatId -> GlobalBotM HalmaChat
loadHalmaChat chatId = do
  botState <- get
  case M.lookup chatId (bsChats botState) of
    Just chat -> pure chat
    Nothing -> do
      persistence <- bcPersistence <$> ask
      liftIO $ fromMaybe dflt <$> bpLoad persistence chatId
  where
    dflt = initialHalmaChat chatId

saveHalmaChat :: HalmaChat -> GlobalBotM ()
saveHalmaChat chat = do
  modify $ \botState ->
    let
      chats = bsChats botState
      chats' = M.insert (hcId chat) chat chats
    in
      botState { bsChats = chats' }
  persistence <- bcPersistence <$> ask
  liftIO $ bpSave persistence chat

withHalmaChat
  :: ChatId
  -> BotM a
  -> GlobalBotM a
withHalmaChat chatId action = do
  logMsg "Loading chat state ..."
  chat <- loadHalmaChat chatId
  logMsg "Loaded chat state."
  (res, chat') <- stateZoom chat action
  logMsg "Saving chat state ..."
  when (chat /= chat') $
    saveHalmaChat chat'
  return res

handleUpdate
  :: TG.Update
  -> GlobalBotM ()
handleUpdate update = do
  logMsg $ "Received update: " ++ show update
  case update of
    TG.Update { TG.update_id = updateId, TG.message = Just msg } -> handleMsg updateId msg
    _ -> logMsg "Update does not contain a valid ID or a message. Ignoring."
  where
    handleMsg updateId msg =
      case msg of
        TG.Message { TG.text = Just txt, TG.date = date, TG.chat = tgChat } ->
          withHalmaChat (fromIntegral (TG.chat_id tgChat)) $ do
            lastUpdateId <- gets hcLastUpdateId
            lastUpdateDate <- gets hcLastUpdateDate
            if updateId <= lastUpdateId && lastUpdateDate + dayInSeconds > date then
              logMsg "Ignoring duplicate update"
            else do
              modify $ \chat -> chat { hcLastUpdateId = updateId, hcLastUpdateDate = date }
              handleTextMsg txt msg
        _ -> logMsg "Not a text message update, ignoring"
    dayInSeconds = 24*60*60

halmaBot :: GlobalBotM ()
halmaBot = do
  updates <- getUpdatesRetry
  mapM_ handleUpdate updates
  halmaBot

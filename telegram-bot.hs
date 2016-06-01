{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Game.Halma.State (HalmaState (..))
import Game.Halma.TelegramBot.BotM
import Game.Halma.TelegramBot.DrawBoard
import Game.Halma.TelegramBot.Types

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Class (MonadState (..), gets, modify)
import Servant.Common.Req (ServantError)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as TG

main :: IO ()
main =
  getArgs >>= \case
    [tokenStr, chatId] -> do
      let token = TG.Token (ensureIsPrefixOf "bot" (T.pack tokenStr))
      evalBotM halmaBot (initialBotState (T.pack chatId) token)
    _ -> do
      hPutStrLn stderr "Usage: ./halma-bot telegram-token chat-id"

ensureIsPrefixOf :: T.Text -> T.Text -> T.Text
ensureIsPrefixOf prefix str =
  if prefix `T.isPrefixOf` str then str else prefix <> str

getUpdates :: BotM (Either ServantError [TG.Update])
getUpdates = do
  nid <- gets bsNextId
  let
    limit = 100
    timeout = 10
    updateReq =
      \token -> TG.getUpdates token (Just nid) (Just limit) (Just timeout)
  runReq updateReq >>= \case
    Left err -> return (Left err)
    Right (TG.UpdatesResponse updates) -> do
      unless (null updates) $ do
        let nid' = 1 + maximum (map TG.update_id updates)
        modify (\s -> s { bsNextId = nid' })
      return (Right updates)
  
sendCurrentBoard :: HalmaState size -> BotM ()
sendCurrentBoard halmaState =
  withRenderedBoardInPngFile (hsBoard halmaState) $ \path -> do
    chatId <- gets bsChatId
    let
      fileUpload = TG.localFileUpload path
      photoReq = TG.uploadPhotoRequest chatId fileUpload
    logErrors $ runReq $ \token -> TG.uploadPhoto token photoReq

type Msg = ChatId -> TG.SendMessageRequest

textMsg :: T.Text -> Msg
textMsg text chatId = TG.sendMessageRequest chatId text

sendMsg :: Msg -> BotM ()
sendMsg createMsg = do
  chatId <- gets bsChatId
  logErrors $ runReq $ \token -> TG.sendMessage token (createMsg chatId)

welcomeMsg :: Msg
welcomeMsg chatId =
  let
    text =
      "Greetings from HalmaBot! This is an open-source bot written in " <>
      "Haskell by Tim Baumann <tim@timbaumann.info>. " <>
      "The source code is available at https://github.com/timjb/halma."
  in
    (TG.sendMessageRequest chatId text)
    { TG.message_disable_web_page_preview = Just True }

helpMsg :: Msg
helpMsg =
  textMsg $
    "You can control HalmaBot by sending these commands:\n" <>
    "/newmatch — starts a new match between two or three players\n" <>
    "/newround — start a new game round\n" <>
    "/help — display this message"

handleCommand :: CmdCall -> BotM (Maybe (BotM ()))
handleCommand cmdCall =
  case cmdCall of
    CmdCall { cmdCallName = "help" } -> do
      sendMsg helpMsg
      pure Nothing
    CmdCall { cmdCallName = "start" } ->
      pure $ Just (sendMsg welcomeMsg)
    CmdCall { cmdCallName = "newmatch" } ->
      pure $ Just $ modify $ \botState ->
        botState { bsMatchState = GatheringPlayers [] }
    CmdCall { cmdCallName = "newround" } ->
      pure $ Just $ sendMsg $ textMsg "todo: newgame"
    _ -> pure Nothing

handleTextMsg :: T.Text -> TG.Message -> BotM (Maybe (BotM ()))
handleTextMsg text fullMsg = do
  matchState <- gets bsMatchState
  case (matchState, text) of
    (_, parseCmdCall -> Just cmdCall) ->
      handleCommand cmdCall
    (GatheringPlayers players, "me") ->
      pure $ Just (addTelegramPlayer players)
    (GatheringPlayers players, "yes, me") ->
      pure $ Just (addTelegramPlayer players)
    (GatheringPlayers players, "an AI") ->
      pure $ Just (addAIPlayer players)
    (GatheringPlayers players, "yes, an AI") ->
      pure $ Just (addAIPlayer players)
    (GatheringPlayers players, "no") ->
      pure $ Just (startMatch players)
    _ -> pure Nothing
  where
    addPlayer :: Player -> [Player] -> BotM ()
    addPlayer player players =
      let
        players' = players ++ [player]
        isFull = length players' == 3
      in do
        botState <- get
        if isFull then
          case newMatch players' of
            Left err -> do
              liftIO $ printError $
                "could not create a new match with 3 players! " <>
                "error message: " <> err
            Right match ->
              put $ botState { bsMatchState = MatchRunning match }
        else
          put $ botState { bsMatchState = GatheringPlayers players' }
    addAIPlayer :: [Player] -> BotM ()
    addAIPlayer = addPlayer AIPlayer
    addTelegramPlayer :: [Player] -> BotM ()
    addTelegramPlayer players =
      case TG.from fullMsg of
        Nothing ->
          sendMsg $ textMsg "cannot add sender of last message as a player!"
        Just user ->
          addPlayer (TelegramPlayer user) players
    startMatch players =
      case newMatch players of
        Left err -> sendMsg (textMsg err)
        Right match -> 
          modify $ \botState ->
            botState { bsMatchState = MatchRunning match }

sendGatheringPlayers :: [Player] -> BotM ()
sendGatheringPlayers players = 
  case players of
    [] ->
      sendMsg $ textMsgWithKeyboard
        "Starting a new match! Who is the first player?"
        meKeyboard
    [firstPlayer] ->
      let
        text =
          "The first player is " <> showPlayer firstPlayer <> ".\n" <>
          "Who is the second player?"
      in
        sendMsg (textMsgWithKeyboard text meKeyboard)
    [firstPlayer,secondPlayer] ->
      let
        text =
          "The first two players are " <> showPlayer firstPlayer <>
          " and " <> showPlayer secondPlayer <> ".\n" <>
          "Is there a third player?"
      in
        sendMsg (textMsgWithKeyboard text anotherPlayerKeyboard)
    _ ->
      liftIO $ printError $
        "unexpected number of players: " <> T.pack (show (length players))
  where
    button txt =
      TG.KeyboardButton
      { TG.kb_text = txt
      , TG.kb_request_contact = Nothing
      , TG.kb_request_location = Nothing
      }
    mkKeyboard buttons =
      TG.ReplyKeyboardMarkup
      { TG.reply_keyboard = buttons
      , TG.reply_resize_keyboard = Just True
      , TG.reply_one_time_keyboard = Just True
      , TG.reply_selective = Just False
      }
    meKeyboard = mkKeyboard [[button "me"], [button "an AI"]]
    anotherPlayerKeyboard =
      mkKeyboard [[button "yes, me"], [button "yes, an AI"], [button "no"]]
    textMsgWithKeyboard text keyboard chatId =
      (TG.sendMessageRequest chatId text)
      { TG.message_reply_markup = Just keyboard }

sendMatchState :: BotM ()
sendMatchState = do
  matchState <- gets bsMatchState
  case matchState of
    NoMatch ->
      sendMsg $ textMsg $
        "Start a new Halma match with /newmatch"
    GatheringPlayers players ->
      sendGatheringPlayers players
    MatchRunning match ->
      case matchCurrentGame match of
        Nothing ->
          sendMsg $ textMsg $
            "Start a new round with /newround"
        Just game ->
          sendCurrentBoard game

halmaBot :: BotM ()
halmaBot = do
  sendMsg welcomeMsg
  mainLoop
  where
    mainLoop :: BotM ()
    mainLoop = do
      sendMatchState
      getUpdatesLoop
      mainLoop
    getUpdatesLoop =
      getUpdates >>= \case
        Left err -> do { printError err; getUpdatesLoop }
        Right updates -> do
          actions <- catMaybes <$> mapM handleUpdate updates
          if null actions then
            getUpdatesLoop
          else do
            sequence_ actions
            mainLoop
    handleUpdate update = do
      liftIO $ print update
      case update of
        TG.Update { TG.message = Just msg } -> handleMsg msg
        _ -> pure Nothing
    handleMsg msg =
      case msg of
        TG.Message { TG.text = Just txt } -> handleTextMsg txt msg
        _ -> pure Nothing

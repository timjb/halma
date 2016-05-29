{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Game.Halma.Board
import Game.Halma.Board.Draw (drawBoard')
import Game.Halma.Configuration (Configuration (..), NumberOfPlayers (..))
import Game.Halma.State (HalmaState (..), newGame)

import Data.Char (isAlphaNum, isSpace)
import Data.Colour.SRGB (sRGB24read)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Diagrams.Backend.Cairo (Cairo, renderCairo)
import Diagrams.Prelude ((#))
import Diagrams.Query (resetValue)
import Diagrams.Size (dims)
import Diagrams.TwoD.Types (V2 (..))
import Control.Monad (unless, guard)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State.Class (MonadState (..), gets, modify)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Common.Req (ServantError)
import System.Directory (getTemporaryDirectory)
import System.Environment (getArgs)
import System.IO (hClose, hPrint, hPutStrLn, stderr)
import System.IO.Temp (withTempFile)
import qualified Data.Text as T
import qualified Diagrams.Prelude as D
import qualified Web.Telegram.API.Bot as TG

data Player
  = AIPlayer
  | TelegramPlayer TG.User
  deriving (Show)

showPlayer :: Player -> T.Text
showPlayer player =
  case player of
    AIPlayer -> "AI"
    TelegramPlayer user ->
      case TG.user_username user of
        Just username -> username
        Nothing ->
          TG.user_first_name user <>
          maybe "" (" " <>) (TG.user_last_name user)

newtype MatchResult
  = MatchResult
  { numberOfMoves :: [(Player, Int)]
  } deriving (Show)

data Match
  = Match
  { matchPlayers :: [Player]
  , matchHistory :: [MatchResult]
  , matchCurrentGame :: Maybe (HalmaState 'S)
  } deriving (Show)

newMatch :: [Player] -> Either T.Text Match
newMatch players =
  let
    config =
      case length players of
        i | i < 2 -> Left "can't start a match with at least two players!"
        2 -> Right (Configuration SmallGrid TwoPlayers)
        3 -> Right (Configuration SmallGrid ThreePlayers)
        _ -> Left "can't start a match with more than three players!"
  in
    mkMatch <$> config
  where
    mkMatch config =
      Match
      { matchPlayers = players
      , matchHistory = []
      , matchCurrentGame = Just (newGame config)
      }

data MatchState
  = NoMatch
  | GatheringPlayers [Player]
  | MatchRunning Match
  deriving (Show)

type ChatId = T.Text

data BotState
  = BotState
  { bsChatId :: ChatId
  , bsNextId :: Int
  , bsToken :: TG.Token
  , bsMatchState :: MatchState
  } deriving (Show)

initialBotState :: T.Text -> TG.Token -> BotState
initialBotState chatId token =
  BotState
  { bsChatId = chatId
  , bsToken = token
  , bsNextId = 0
  , bsMatchState = NoMatch
  }

newtype BotM a
  = BotM
  { unBotM :: ReaderT Manager (StateT BotState IO) a
  } deriving
    ( Functor, Applicative, Monad
    , MonadIO, MonadThrow, MonadCatch, MonadMask
    , MonadState BotState, MonadReader Manager
    )

evalBotM :: BotM a -> BotState -> IO a
evalBotM action initialState = do
  manager <- newManager tlsManagerSettings
  evalStateT (runReaderT (unBotM action) manager) initialState

runReq :: (TG.Token -> Manager -> IO a) -> BotM a
runReq reqAction = do
  manager <- ask
  token <- gets bsToken
  liftIO (reqAction token manager)

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
  let limit = 100
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

printError :: (MonadIO m, Show a) => a -> m ()
printError val = liftIO (hPrint stderr val)

logErrors :: BotM (Either ServantError a) -> BotM ()
logErrors action =
  action >>= \case
    Left err -> printError err
    Right _res -> return ()
  
sendCurrentBoard :: HalmaState 'S -> BotM ()
sendCurrentBoard halmaState =
  withTempPngFilePath $ \path -> do
    chatId <- gets bsChatId
    let board = hsBoard halmaState
        dia = drawBoard' (getGrid board) (drawField board)
        bounds = dims (V2 1000 1000)
    liftIO $ renderCairo path bounds (resetValue dia)
    let fileUpload = TG.localFileUpload path
        photoReq = TG.uploadPhotoRequest chatId fileUpload
    logErrors $ runReq $ \token -> TG.uploadPhoto token photoReq
  where
    withTempPngFilePath handler = do
      systemTempDir <- liftIO getTemporaryDirectory
      withTempFile systemTempDir "halma.png" $ \filePath fileHandle -> do
        liftIO (hClose fileHandle)
        handler filePath
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
    drawField
      ::  HalmaBoard 'S -> (Int, Int) -> D.Diagram Cairo
    drawField board field =
      maybe mempty drawPiece $ lookupHalmaBoard field board

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

data CmdCall
  = CmdCall
  { cmdCallName :: T.Text
  , cmdCallArgs :: Maybe T.Text
  } deriving (Show, Eq)

parseCmdCall :: T.Text -> Maybe CmdCall
parseCmdCall str = do
  guard $ not (T.null str)
  guard $ T.head str == '/'
  let rest = T.tail str
      isCmdChar c = isAlphaNum c || c `elem` ("_-" :: String)
      (cmdName, rest') = T.span isCmdChar rest
  guard $ not (T.null cmdName)
  let rest'' = T.dropWhile isSpace rest'
      args = if T.null rest'' then Nothing else Just rest''
  pure $ CmdCall { cmdCallName = cmdName, cmdCallArgs = args }

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

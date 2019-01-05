{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Game.Halma.TelegramBot.Model.Types (HalmaChat (..))
import Game.Halma.TelegramBot.Controller (handleUpdate)
import Game.Halma.TelegramBot.Controller.BotM (evalGlobalBotM)
import Game.Halma.TelegramBot.Controller.Types (BotPersistence (..), BotConfig (..))

import Aws.Lambda.Configuration (LambdaOptions(..), getRecord, returnAndFail, returnAndSucceed)
import Control.Lens (set, view)
import Control.Monad (void)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Monoid ((<>))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)
import System.IO (stdout)
import qualified Control.Monad.Trans.AWS as AWS (runAWST)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Network.AWS as AWS
import qualified Network.AWS.S3.Types as S3
import qualified Network.AWS.S3.GetObject as S3
import qualified Network.AWS.S3.PutObject as S3
import qualified Web.Telegram.API.Bot as TG

descr :: T.Text
descr =
  "Halma AWS Lambda Executable\n\n" <>
  "This serverless chatbot executable expects\n" <>
  "  - an update event from the Telegram Bot API, provided as JSON via stdin\n" <>
  "  - the Telegram API token in the environment variable TELEGRAM_TOKEN\n" <>
  "  - the name of the S3 bucket where to save the games in the environment variable HALMA_S3_BUCKET\n" <>
  "It will do some calls to the Telegram API and then exit."

mkTelegramToken :: String -> TG.Token
mkTelegramToken = TG.Token . ensureIsPrefixOf "bot" . T.pack
  where
    ensureIsPrefixOf :: T.Text -> T.Text -> T.Text
    ensureIsPrefixOf prefix str =
      if prefix `T.isPrefixOf` str then
        str
      else
        prefix <> str

s3Persistence :: S3.BucketName -> IO BotPersistence
s3Persistence bucket = do
  lgr <- AWS.newLogger AWS.Debug stdout
  env <- set AWS.envLogger lgr <$> AWS.newEnv AWS.Discover
  pure $
    BotPersistence
    { bpSave =
        \chat -> do
          let
            body = AWS.toBody (A.encode chat)
            req = S3.putObject bucket (objectKey (hcId chat)) body
          runResourceT $ AWS.runAWST env $
            void $ AWS.send req
    , bpLoad =
        \chatId -> do
          let
            req = S3.getObject bucket (objectKey chatId)
          runResourceT $ AWS.runAWST env $ do
            errOrRes <- AWS.trying S3._NoSuchKey $ AWS.send req
            case errOrRes of
              Left _noSuchKeyErr -> pure Nothing
              Right res -> do
                json <- view S3.gorsBody res `AWS.sinkBody` sinkParser A.json'
                case A.fromJSON json of
                  A.Error e -> fail e
                  A.Success a -> pure a
    }
  where
    objectKey chatId = S3.ObjectKey (T.pack (show chatId) <> ".json")

updateHandler :: TG.Update -> IO (Either String ())
updateHandler update = do
  manager <- newManager tlsManagerSettings
  token <- mkTelegramToken <$> getEnv "TELEGRAM_TOKEN"
  bucket <- S3.BucketName . T.pack <$> getEnv "HALMA_S3_BUCKET"
  persistence <- s3Persistence bucket
  let
    cfg =
      BotConfig
        { bcToken = token
        , bcPersistence = persistence
        , bcManager = manager
        }
  evalGlobalBotM (handleUpdate update) cfg

main :: IO ()
main = do
  LambdaOptions { functionHandler, eventObject, executionUuid } <- getRecord descr
  case functionHandler of
    "update" -> do
       case A.eitherDecode $ BSL.fromStrict $ Encoding.encodeUtf8 eventObject of
         Left err ->
           returnAndFail executionUuid $ "Error while decoding JSON: " ++ err
         Right update -> do
           res <- updateHandler update
           either (returnAndFail executionUuid) (returnAndSucceed executionUuid) res
    _ -> returnAndFail executionUuid $ "Handler '" <> functionHandler <> "' does not exist!"
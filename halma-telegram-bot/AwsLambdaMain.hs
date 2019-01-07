{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

{-
  Halma AWS Lambda Executable
  This serverless chatbot executable expects
    - an update event from the Telegram Bot API, provided as JSON via stdin
    - the Telegram API token in the environment variable TELEGRAM_TOKEN
    - the name of the S3 bucket where to save the games in the environment variable HALMA_S3_BUCKET
  It will do some calls to the Telegram API and then exit.
-}

import Game.Halma.TelegramBot.Model.Types (HalmaChat (..))
import Game.Halma.TelegramBot.Controller (handleUpdate)
import Game.Halma.TelegramBot.Controller.BotM (evalGlobalBotM)
import Game.Halma.TelegramBot.Controller.Types (BotPersistence (..), BotConfig (..))

import AWS.Lambda.Runtime (ioRuntime)
import Control.Exception (SomeException(..), handle)
import Control.Lens (set, view)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Monoid ((<>))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)
import System.IO (stdout)
import qualified Control.Monad.Trans.AWS as AWS (runAWST)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Network.AWS as AWS
import qualified Network.AWS.S3.Types as S3
import qualified Network.AWS.S3.GetObject as S3
import qualified Network.AWS.S3.PutObject as S3
import qualified Web.Telegram.API.Bot as TG

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
              Left _noSuchKeyErr -> do
                liftIO $ putStrLn $ "Chat with ID " ++ show chatId ++ " has not been saved to S3 yet."
                pure Nothing
              Right res -> do
                json <- view S3.gorsBody res `AWS.sinkBody` sinkParser A.json'
                case A.fromJSON json of
                  A.Error e -> fail e
                  A.Success a -> pure a
    }
  where
    objectKey chatId = S3.ObjectKey (T.pack (show chatId) <> ".json")

updateHandler :: TG.Update -> IO (Either String ())
updateHandler update = handle exceptionHandler $ do
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
  where
    exceptionHandler :: SomeException -> IO (Either String ())
    exceptionHandler (SomeException e) = do
      putStrLn $ "Caught exception: " ++ show e
      return $ Left $ show e

main :: IO ()
main = ioRuntime updateHandler
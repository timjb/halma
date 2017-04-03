module Game.Halma.TelegramBot.Controller.Persistence
  ( noPersistence
  , filePersistence
  ) where

import Game.Halma.TelegramBot.Model.Types (HalmaChat (..))
import Game.Halma.TelegramBot.Controller.Types (BotPersistence (..))

import System.Directory (doesFileExist, getTemporaryDirectory, renameFile)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (openTempFile)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as LBS

noPersistence :: BotPersistence
noPersistence =
  BotPersistence
  { bpSave = const (return ())
  , bpLoad = const (return Nothing)
  }

filePersistence :: FilePath -> BotPersistence
filePersistence outDir =
  BotPersistence
  { bpSave =
      \chat -> do
        let fileName = show (hcId chat) ++ ".json"
            fullFilePath = outDir </> fileName
        systemTempDir <- getTemporaryDirectory
        (tmpFilePath, tmpFileHandle) <- openTempFile systemTempDir fileName
        LBS.hPut tmpFileHandle (A.encodePretty chat)
        hClose tmpFileHandle
        renameFile tmpFilePath fullFilePath
  , bpLoad =
      \chatId -> do
        let filePath = outDir </> (show chatId ++ ".json")
        fileExists <- doesFileExist filePath
        if not fileExists then
          pure Nothing
        else do
          jsonLBS <- LBS.readFile filePath
          case A.eitherDecode jsonLBS of
            Left err ->
              fail $ "decoding " ++ filePath ++ " failed: " ++ err
            Right chat ->
              pure (Just chat)
  }
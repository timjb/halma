module Game.Halma.TelegramBot.Cmd
  ( CmdCall (..)
  , parseCmdCall
  ) where

import Control.Monad (guard)
import Data.Char (isAlphaNum, isSpace)

import qualified Data.Text as T

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
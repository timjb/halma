{-# LANGUAGE OverloadedStrings #-}

module Game.Halma.TelegramBot.View.Pretty
  ( prettyUser
  , prettyPlayer
  , prettyLocaleId
  , parsePrettyLocaleId
  , teamEmoji
  ) where

import Game.Halma.Board
import Game.Halma.TelegramBot.Model.Types

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as TG

prettyUser :: TG.User -> T.Text
prettyUser user =
  case TG.user_username user of
    Just username -> "@" <> username
    Nothing ->
      TG.user_first_name user <>
      maybe "" (" " <>) (TG.user_last_name user)

prettyPlayer :: Player -> T.Text
prettyPlayer player =
  case player of
    AIPlayer -> "AI"
    TelegramPlayer user -> prettyUser user

prettyLocaleId :: LocaleId -> T.Text
prettyLocaleId localeId = 
  case localeId of
    De -> "\127465\127466 de" -- german flag :de:
    En -> "\127468\127463 en" -- union jack :gb:

parsePrettyLocaleId :: T.Text -> Maybe LocaleId
parsePrettyLocaleId s =
  lookup (T.strip s) table
  where
    table = map (\locId -> (prettyLocaleId locId, locId)) [minBound..maxBound]
    
teamEmoji :: Team -> T.Text
teamEmoji dir =
  case dir of
    North     -> "\128309" -- :large_blue_circle: for blue
    Northeast -> "\128154" -- :green_heart: for green
    Northwest -> "\128156" -- :purple_heart: for purple
    South     -> "\128308" -- :red_circle: for red
    Southeast -> "\9899"   -- :medium_black_circle: for black
    Southwest -> "\128310" -- :large_orange_diamond: for orange
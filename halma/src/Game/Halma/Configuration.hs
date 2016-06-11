{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Halma.Configuration
  ( HalmaPlayers (..)
  , Configuration, configurationGrid, configurationPlayers
  , configuration
  , twoPlayersOnSmallGrid, threePlayersOnSmallGrid
  , playersOnLargeGrid
  , setSmallGrid, setLargeGrid
  , addPlayerToConfig
  , newGame
  ) where

import Game.Halma.Board
import Game.TurnCounter

import Data.Aeson ((.=), (.:))
import Data.Foldable (toList)
import qualified Data.Aeson as A

data HalmaPlayers a
  = TwoPlayers a a
  | ThreePlayers a a a
  | FourPlayers a a a a
  | FivePlayers a a a a a
  | SixPlayers a a a a a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance A.ToJSON a => A.ToJSON (HalmaPlayers a) where
  toJSON = A.toJSON . toList

instance A.FromJSON a => A.FromJSON (HalmaPlayers a) where
  parseJSON val = do
    parsedPlayers <- A.parseJSON val
    case parsedPlayers of
      [a,b] -> pure (TwoPlayers a b)
      [a,b,c] -> pure (ThreePlayers a b c)
      [a,b,c,d] -> pure (FourPlayers a b c d)
      [a,b,c,d,e] -> pure (FivePlayers a b c d e)
      [a,b,c,d,e,f] -> pure (SixPlayers a b c d e f)
      _ ->
        fail $ "unexpected count of players for a Halma board: " ++ show (length parsedPlayers)

getPlayers :: HalmaPlayers a -> [(Team, a)]
getPlayers halmaPlayers =
  case halmaPlayers of
    TwoPlayers a b ->
      [(North, a), (South, b)]
    ThreePlayers a b c ->
      [(Northeast, a), (South, b), (Northwest, c)]
    FourPlayers a b c d ->
      [(Northeast, a), (Southeast, b), (Southwest, c), (Northwest, d)]
    FivePlayers a b c d e ->
      [(Northeast, a), (Southeast, b), (South, c), (Southwest, d), (Northwest, e)]
    SixPlayers a b c d e f ->
      [(North, a), (Northeast, b), (Southeast, c), (South, d), (Southwest, e), (Northwest, f)]

data Configuration a
  = Configuration
  { configurationGrid :: HalmaGrid
  , configurationPlayers :: HalmaPlayers a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

configuration :: HalmaGrid -> HalmaPlayers a -> Maybe (Configuration a)
configuration grid players =
  if grid /= SmallGrid || length players <= 3 then
    Just
      Configuration
        { configurationGrid = grid
        , configurationPlayers = players
        }
  else
    Nothing

instance A.ToJSON a => A.ToJSON (Configuration a) where
  toJSON config =
    A.object
      [ "grid" .= configurationGrid config
      , "players" .= configurationPlayers config
      ]

instance A.FromJSON a => A.FromJSON (Configuration a) where
  parseJSON =
    A.withObject "Configuration" $ \o -> do
      grid <- o .: "grid"
      players <- o .: "players"
      case configuration grid players of
        Nothing -> fail "too many players for small grid!"
        Just config -> pure config

twoPlayersOnSmallGrid :: a -> a -> Configuration a
twoPlayersOnSmallGrid a b = Configuration SmallGrid (TwoPlayers a b)

threePlayersOnSmallGrid :: a -> a -> a -> Configuration a
threePlayersOnSmallGrid a b c = Configuration SmallGrid (ThreePlayers a b c)

playersOnLargeGrid :: HalmaPlayers a -> Configuration a
playersOnLargeGrid players = Configuration LargeGrid players

setSmallGrid :: Configuration a -> Maybe (Configuration a)
setSmallGrid config =
  if length (configurationPlayers config) <= 3 then
    Just (config { configurationGrid = SmallGrid })
  else
    Nothing

setLargeGrid :: Configuration a -> Configuration a
setLargeGrid config =
  config { configurationGrid = LargeGrid }

addPlayerToConfig :: a -> Configuration a -> Configuration a
addPlayerToConfig new config =
  case configurationPlayers config of
    TwoPlayers a b ->
      Configuration (configurationGrid config) (ThreePlayers a b new)
    ThreePlayers a b c ->
      Configuration LargeGrid (FourPlayers a b c new)
    FourPlayers a b c d ->
      Configuration LargeGrid (FivePlayers a b c d new)
    FivePlayers a b c d e ->
      Configuration LargeGrid (SixPlayers a b c d e new)
    SixPlayers {} -> config

newGame :: Configuration a -> (HalmaBoard, TurnCounter (Team, a))
newGame config =
  ( initialBoard (configurationGrid config) isActive
  , newTurnCounter parties
  )
  where
    parties = getPlayers (configurationPlayers config)
    isActive color = color `elem` (fst <$> parties)

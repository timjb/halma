{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Halma.Configuration
  ( HalmaPlayers (..)
  , twoPlayers, threePlayers, fourPlayers, fivePlayers, sixPlayers
  , getPlayers
  , Configuration (..)
  , defaultConfiguration
  , SomeConfiguration (..)
  ) where

import Game.Halma.Board

import Data.Aeson ((.=), (.:))
import Data.Foldable (toList)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

data HalmaPlayers :: HalmaGridSize -> * -> * where
  TwoPlayers   :: a -> a ->                     HalmaPlayers size a
  ThreePlayers :: a -> a -> a ->                HalmaPlayers size a
  FourPlayers  :: a -> a -> a -> a ->           HalmaPlayers 'L   a
  FivePlayers  :: a -> a -> a -> a -> a ->      HalmaPlayers 'L   a
  SixPlayers   :: a -> a -> a -> a -> a -> a -> HalmaPlayers 'L   a

deriving instance Eq a => Eq (HalmaPlayers size a)
deriving instance Show a => Show (HalmaPlayers size a)
deriving instance Functor (HalmaPlayers size)
deriving instance Foldable (HalmaPlayers size)
deriving instance Traversable (HalmaPlayers size)

instance A.ToJSON a => A.ToJSON (HalmaPlayers size a) where
  toJSON = A.toJSON . toList

instance A.FromJSON a => A.FromJSON (HalmaPlayers 'S a) where
  parseJSON val = do
    parsedPlayers <- A.parseJSON val
    case parsedPlayers of
      [a,b] -> pure (TwoPlayers a b)
      [a,b,c] -> pure (ThreePlayers a b c)
      _ ->
        fail $ "unexpected count of players for a small board: " ++ show (length parsedPlayers)

instance A.FromJSON a => A.FromJSON (HalmaPlayers 'L a) where
  parseJSON val = do
    parsedPlayers <- A.parseJSON val
    case parsedPlayers of
      [a,b] -> pure (TwoPlayers a b)
      [a,b,c] -> pure (ThreePlayers a b c)
      [a,b,c,d] -> pure (FourPlayers a b c d)
      [a,b,c,d,e] -> pure (FivePlayers a b c d e)
      [a,b,c,d,e,f] -> pure (SixPlayers a b c d e f)
      _ ->
        fail $ "unexpected count of players for a large board: " ++ show (length parsedPlayers)

twoPlayers, threePlayers :: HalmaPlayers size ()
twoPlayers = TwoPlayers () ()
threePlayers = ThreePlayers () () ()
fourPlayers, fivePlayers, sixPlayers :: HalmaPlayers 'L ()
fourPlayers = FourPlayers () () () ()
fivePlayers = FivePlayers () () () () ()
sixPlayers = SixPlayers () () () () () ()

getPlayers :: HalmaPlayers size a -> [(Team, a)]
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

data Configuration size a
  = Configuration
  { configurationGrid :: HalmaGrid size
  , configurationPlayers :: HalmaPlayers size a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance A.ToJSON a => A.ToJSON (Configuration size a) where
  toJSON config =
    A.object
      [ "grid" .= configurationGrid config
      , "players" .= configurationPlayers config
      ]

parseConfigurationFromJSON
  :: (A.FromJSON a, A.FromJSON (HalmaGrid size), A.FromJSON (HalmaPlayers size a))
  => A.Value
  -> A.Parser (Configuration size a)
parseConfigurationFromJSON =
  A.withObject "Configuration size" $ \o -> do
    grid <- o .: "grid"
    players <- o .: "players"
    pure $
      Configuration
        { configurationGrid = grid
        , configurationPlayers = players
        }

instance A.FromJSON a => A.FromJSON (Configuration 'S a) where
  parseJSON = parseConfigurationFromJSON

instance A.FromJSON a => A.FromJSON (Configuration 'L a) where
  parseJSON = parseConfigurationFromJSON

defaultConfiguration :: Configuration 'S ()
defaultConfiguration = Configuration SmallGrid twoPlayers

data SomeConfiguration a = forall size. SomeConfiguration (Configuration size a)

instance Show a => Show (SomeConfiguration a) where
  show (SomeConfiguration a) = "SomeConfiguration (" ++ show a ++ ")"

instance Show a => Eq (SomeConfiguration a) where
  SomeConfiguration a == SomeConfiguration b = show a == show b

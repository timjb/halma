{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Game.Halma.Configuration
  ( NumberOfPlayers (..)
  , getPlayers
  , Configuration (..)
  , defaultConfiguration
  , SomeConfiguration (..)
  ) where

import Game.Halma.Board

data NumberOfPlayers :: HalmaGridSize -> * where
  TwoPlayers   :: NumberOfPlayers size
  ThreePlayers :: NumberOfPlayers size
  FourPlayers  :: NumberOfPlayers 'L
  FivePlayers  :: NumberOfPlayers 'L
  SixPlayers   :: NumberOfPlayers 'L

deriving instance Show (NumberOfPlayers size)

instance Eq (NumberOfPlayers size) where
  a == b = show a == show b

getPlayers :: NumberOfPlayers size -> [Team]
getPlayers numberOfPlayers =
  case numberOfPlayers of
    TwoPlayers   -> [North, South]
    ThreePlayers -> [Northeast, South, Northwest]
    FourPlayers  -> [Northeast, Southeast, Southwest, Northwest]
    FivePlayers  -> [Northeast, Southeast, South, Southwest, Northwest]
    SixPlayers   -> [minBound..maxBound]

data Configuration :: HalmaGridSize -> * where
  Configuration :: HalmaGrid size -> NumberOfPlayers size -> Configuration size

deriving instance Show (Configuration size)

instance Eq (Configuration size) where
  a == b = show a == show b

defaultConfiguration :: Configuration 'S
defaultConfiguration = Configuration SmallGrid TwoPlayers

data SomeConfiguration = forall size. SomeConfiguration (Configuration size)

instance Show SomeConfiguration where
  show (SomeConfiguration a) = "SomeConfiguration (" ++ show a ++ ")"

instance Eq SomeConfiguration where
  SomeConfiguration a == SomeConfiguration b = show a == show b

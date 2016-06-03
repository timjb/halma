{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Game.Halma.Configuration
  ( HalmaPlayers (..)
  , twoPlayers, threePlayers, fourPlayers, fivePlayers, sixPlayers
  , getPlayers
  , Configuration (..)
  , defaultConfiguration
  , SomeConfiguration (..)
  ) where

import Game.Halma.Board

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

defaultConfiguration :: Configuration 'S ()
defaultConfiguration = Configuration SmallGrid twoPlayers

data SomeConfiguration a = forall size. SomeConfiguration (Configuration size a)

instance Show a => Show (SomeConfiguration a) where
  show (SomeConfiguration a) = "SomeConfiguration (" ++ show a ++ ")"

instance Show a => Eq (SomeConfiguration a) where
  SomeConfiguration a == SomeConfiguration b = show a == show b

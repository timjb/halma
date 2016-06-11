{-# LANGUAGE DeriveFunctor #-}

module Game.TurnCounter
  ( TurnCounter (..)
  , newTurnCounter
  , nextTurn
  , previousTurn
  , currentPlayer
  , nextPlayer
  , previousPlayer
  , currentRound
  ) where

data TurnCounter p
  = TurnCounter
  { tcPlayers :: [p]
  , tcCounter :: Int
  } deriving (Eq, Show, Functor)

newTurnCounter :: [p] -> TurnCounter p
newTurnCounter players =
  TurnCounter
    { tcPlayers = players
    , tcCounter = 0
    }

nextTurn :: TurnCounter p -> TurnCounter p
nextTurn (TurnCounter ps c) = TurnCounter ps (c+1)

previousTurn :: TurnCounter p -> TurnCounter p
previousTurn (TurnCounter ps c) = TurnCounter ps (c-1)

currentPlayer :: TurnCounter p -> p
currentPlayer (TurnCounter ps c) = ps !! (c `mod` length ps)

nextPlayer :: TurnCounter p -> p
nextPlayer = currentPlayer . nextTurn

previousPlayer :: TurnCounter p -> p
previousPlayer = currentPlayer . previousTurn

currentRound :: TurnCounter p -> Int
currentRound (TurnCounter ps c) = c `div` length ps

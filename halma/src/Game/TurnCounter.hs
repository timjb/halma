{-# LANGUAGE DeriveFunctor #-}

module Game.TurnCounter
  ( TurnCounter (..)
  , newTurnCounter
  , nextTurn, nextTurnWith
  , previousTurn, previousTurnWith
  , currentPlayer
  , nextPlayer
  , previousPlayer
  , currentRound
  ) where

import Data.List (find)

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

nextTurnWith :: (p -> Bool) -> TurnCounter p -> Maybe (TurnCounter p)
nextTurnWith predicate tc =
  find (predicate . currentPlayer) $
  take (length (tcPlayers tc)) $
  iterate nextTurn (nextTurn tc)

previousTurn :: TurnCounter p -> TurnCounter p
previousTurn (TurnCounter ps c) = TurnCounter ps (c-1)

previousTurnWith :: (p -> Bool) -> TurnCounter p -> Maybe (TurnCounter p)
previousTurnWith predicate tc =
  find (predicate . currentPlayer) $
  take (length (tcPlayers tc)) $
  iterate previousTurn (previousTurn tc)

currentPlayer :: TurnCounter p -> p
currentPlayer (TurnCounter ps c) = ps !! (c `mod` length ps)

nextPlayer :: TurnCounter p -> p
nextPlayer = currentPlayer . nextTurn

previousPlayer :: TurnCounter p -> p
previousPlayer = currentPlayer . previousTurn

currentRound :: TurnCounter p -> Int
currentRound (TurnCounter ps c) = c `div` length ps

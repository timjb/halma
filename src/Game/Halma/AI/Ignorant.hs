module Game.Halma.AI.Ignorant
  ( aiMove
  ) where

import Game.Halma.Board
import Game.Halma.Rules
import Game.Halma.AI.Base

import Data.List (maximumBy)
import Data.Ord (comparing)

ignorantFixedDepth :: Int -> RuleOptions -> Team -> HalmaBoard size -> Rating
ignorantFixedDepth n opts team board =
  if n <= 0 then
    rateTeam team board
  else if hasFinished board team then
    WinIn 0
  else
    beingSomewhatGreedy (rateTeam team board) $ maximum $
    map (ignorantFixedDepth (n-1) opts team . outcome board) $
    allLegalMoves opts board team

beingSomewhatGreedy :: Rating -> Rating -> Rating
beingSomewhatGreedy x y =
  case (x, y) of
    (WinIn n, _)  -> WinIn n
    (LossIn n, _) -> LossIn n
    (_, WinIn n)  -> WinIn (n+1)
    (_, LossIn n) -> LossIn (n+1)
    (Rating a, Rating b) ->
      let greedyness = 0.1
      in Rating (greedyness*a + (1-greedyness)*b)

aiMove :: RuleOptions -> HalmaBoard size -> Team -> Move
aiMove opts board team =
  if null legalMoves then
    error "There is no legal move."
  else
    maximumBy (comparing $ finalRating team . outcome board) legalMoves
  where
    legalMoves = allLegalMoves opts board team
    finalRating = ignorantFixedDepth 1 opts

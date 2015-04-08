module Game.Halma.AI.Ignorant
  ( aiMove
  ) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Game.Halma.Board
import Game.Halma.Rules
import Game.Halma.AI.Base


ignorantFixedDepth :: Int -> RuleOptions -> Team -> HalmaBoard size -> Rating
ignorantFixedDepth n opts team board
  | n<=0  = teamRating team board
  | hasFinished board team  = WinIn 0
  | otherwise  = beingSomewhatGreedy (teamRating team board) $ maximum $
      map (ignorantFixedDepth (n-1) opts team . outcome board) $
          allLegalMoves opts board team

beingSomewhatGreedy :: Rating -> Rating -> Rating
beingSomewhatGreedy (WinIn n) _ = WinIn n
beingSomewhatGreedy (LossIn n) _ = LossIn n
beingSomewhatGreedy _ (WinIn n) = WinIn (n+1)
beingSomewhatGreedy _ (LossIn n) = LossIn (n+1)
beingSomewhatGreedy (Rating a) (Rating b) = Rating $ greedyness*a + (1-greedyness)*b
  where greedyness = 0.1

aiMove :: RuleOptions -> HalmaBoard size -> Team -> Move size
aiMove opts board team = if null legalMoves then error "There is no legal move."
                         else maximumBy (comparing $ finalRating team . outcome board) legalMoves
  where legalMoves = allLegalMoves opts board team
        finalRating = ignorantFixedDepth 1 opts

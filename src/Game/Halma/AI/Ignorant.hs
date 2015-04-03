module Game.Halma.AI.Ignorant
  ( aiMove
  ) where

import Math.Geometry.Grid hiding (null)
import Data.List (maximumBy, sort)
import Data.Ord (comparing)
import Game.Halma.Board
import Game.Halma.Rules
import Game.Halma.AI.Base


weighting :: Team -> HalmaBoard size -> Weighting
weighting team board  | hasFinished board team  = WinIn 0
                      | otherwise  = Weighting . negate . sum $
                          zipWith (*) pieceWeightingList (sort distances)
  where grid = getGrid board
        pieceWeightingList = replicate 12 2 ++ [3..5]
        distances = map (fromIntegral . distance grid (endCorner grid team)) $
                  allOwnPieces board team

ignorantFixedDepth :: Int -> RuleOptions -> Team -> HalmaBoard size -> Weighting
ignorantFixedDepth n opts team board
  | n<=0  = weighting team board
  | hasFinished board team  = WinIn 0
  | otherwise  = beingSomewhatGreedy (weighting team board) $ maximum $
      map (ignorantFixedDepth (n-1) opts team . outcome board) $
          allLegalMoves opts board team

beingSomewhatGreedy :: Weighting -> Weighting -> Weighting
beingSomewhatGreedy (WinIn n) _ = WinIn n
beingSomewhatGreedy (LossIn n) _ = LossIn n
beingSomewhatGreedy _ (WinIn n) = WinIn (n+1)
beingSomewhatGreedy _ (LossIn n) = LossIn (n+1)
beingSomewhatGreedy (Weighting a) (Weighting b) = Weighting $ greedyness*a + (1-greedyness)*b
  where greedyness = 0.1

aiMove :: RuleOptions -> HalmaBoard size -> Team -> Move size
aiMove opts board team = if null legalMoves then error "There is no legal move."
                         else maximumBy (comparing $ finalRating team . outcome board) legalMoves
  where legalMoves = allLegalMoves opts board team
        finalRating = ignorantFixedDepth 1 opts

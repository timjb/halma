module Game.Halma.AI
  ( aiMove
  ) where

import Math.Geometry.Grid hiding (null)
import Data.List (maximumBy, sort)
import Data.Ord (comparing)
import Game.Halma.Board
import Game.Halma.Rules


type Move size = (Index (HalmaGrid size), Index (HalmaGrid size))

outcome :: HalmaBoard size -> Move size -> HalmaBoard size
outcome board (source, destination) =
  case movePiece source destination board of
    Left err -> error $ "cannot compute outcome: " ++ err
    Right board' -> board'

data Weighting = WinIn Int
               | Weighting Float
               | LossIn Int
  deriving (Show, Eq)

instance Ord Weighting where
  compare (WinIn n) (WinIn m) = compare m n
  compare (WinIn _) _ = GT
  compare _ (WinIn _) = LT
  compare (LossIn n) (LossIn m) = compare n m
  compare (LossIn _) _ = LT
  compare _ (LossIn _) = GT
  compare (Weighting a) (Weighting b) = compare a b


allOwnPieces :: HalmaBoard size -> Team -> [Index (HalmaGrid size)]
allOwnPieces board team = 
    filter ((Just team ==) . flip lookupHalmaBoard board)
    $ indices (getGrid board)

allLegalMoves :: RuleOptions -> HalmaBoard size -> Team -> [Move size]
allLegalMoves opts board team =
  concatMap
    ( \piecePos -> [(piecePos, destination)
                    | destination <- possibleMoves opts board piecePos] )
    $ allOwnPieces board team

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

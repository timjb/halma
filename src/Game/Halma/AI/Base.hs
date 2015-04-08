module Game.Halma.AI.Base
  ( Move
  , outcome
  , Rating(..)
  , teamRating
  , allOwnPieces
  , allLegalMoves
  ) where

import Math.Geometry.Grid
import qualified Data.Map.Strict as M
import Data.List (sort)
import Game.Halma.Board
import Game.Halma.Rules


type Move size = (Index (HalmaGrid size), Index (HalmaGrid size))

outcome :: HalmaBoard size -> Move size -> HalmaBoard size
outcome board (source, destination) =
  case movePiece source destination board of
    Left err -> error $ "cannot compute outcome: " ++ err
    Right board' -> board'

data Rating = WinIn Int
            | Rating Float
            | LossIn Int
  deriving (Show, Eq)

instance Ord Rating where
  compare (WinIn n) (WinIn m) = compare m n
  compare (WinIn _) _ = GT
  compare _ (WinIn _) = LT
  compare (LossIn n) (LossIn m) = compare n m
  compare (LossIn _) _ = LT
  compare _ (LossIn _) = GT
  compare (Rating a) (Rating b) = compare a b

instance Bounded Rating where
  maxBound = WinIn 0
  minBound = LossIn 0


teamRating :: Team -> HalmaBoard size -> Rating
teamRating team board  | hasFinished board team  = WinIn 0
                       | otherwise  = Rating . negate . sum $
                           zipWith (*) pieceWeightingList (sort distances)
  where distances = map (fromIntegral . distance grid (endCorner grid team)) $
                        allOwnPieces board team
        grid = getGrid board
        pieceWeightingList = replicate 12 2 ++ [3..5]

allOwnPieces :: HalmaBoard size -> Team -> [Index (HalmaGrid size)]
allOwnPieces board team = 
  map fst $ filter ((team ==) . snd) $
              M.assocs (toMap board)

allLegalMoves :: RuleOptions -> HalmaBoard size -> Team -> [Move size]
allLegalMoves opts board team =
  concatMap
    ( \piecePos -> [(piecePos, destination)
                    | destination <- possibleMoves opts board piecePos] )
    $ allOwnPieces board team

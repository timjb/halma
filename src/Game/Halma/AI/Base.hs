module Game.Halma.AI.Base
  ( Move
  , outcome
  , Rating (..)
  , rateTeam
  , allOwnPieces
  , allLegalMoves
  ) where

import Game.Halma.Board
import Game.Halma.Rules

import Data.List (sort)
import Math.Geometry.Grid
import qualified Data.Map.Strict as M

type Move size = (Index (HalmaGrid size), Index (HalmaGrid size))

outcome :: HalmaBoard size -> Move size -> HalmaBoard size
outcome board (source, destination) =
  case movePiece source destination board of
    Left err -> error $ "cannot compute outcome: " ++ err
    Right board' -> board'

data Rating
  = WinIn Int
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

rateTeam :: Team -> HalmaBoard size -> Rating
rateTeam team board
  | hasFinished board team =
      WinIn 0
  | otherwise =
      Rating . negate . sum $ zipWith (*) pieceWeightingList (sort distances)
  where
    grid = getGrid board
    distanceToEndCorner = fromIntegral . distance grid (endCorner grid team)
    distances = map distanceToEndCorner (allOwnPieces board team)
    pieceWeightingList = replicate 12 2 ++ [3..5]

allOwnPieces :: HalmaBoard size -> Team -> [Index (HalmaGrid size)]
allOwnPieces board team =
  map fst $ filter (isMyPiece . snd) $ M.assocs (toMap board)
  where
    isMyPiece piece = pieceTeam piece == team

allLegalMoves :: RuleOptions -> HalmaBoard size -> Team -> [Move size]
allLegalMoves opts board team = do
  piecePos <- allOwnPieces board team
  destination <- possibleMoves opts board piecePos
  return (piecePos, destination)

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Halma.Board
  ( HalmaGrid (..)
  , northCorner, southCorner
  , northeastCorner, northwestCorner
  , southeastCorner, southwestCorner
  , corners
  , Team (..)
  , oppositeTeam
  , startCorner, endCorner
  , startFields, endFields
  , twoPlayers, threePlayers
  , HalmaBoard, toMap, fromMap
  , lookupHalmaBoard
  , movePiece
  , initialBoard
  ) where

import GHC.Generics (Generic)
import Math.Geometry.Grid
import Math.Geometry.Grid.Hexagonal
import Math.Geometry.Grid.HexagonalInternal (HexDirection (..), UnboundedHexGrid (..))
import Math.Geometry.GridInternal
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

data HalmaGrid = HalmaGrid
  deriving (Eq, Show, Read, Ord, Bounded, Generic)

corner :: (HexDirection, HexDirection) -> (Int, Int)
corner d = iter4 (intoDirection d) center
  where neighbour' dir = fromJust . flip (neighbour UnboundedHexGrid) dir
        intoDirection (d1, d2) = neighbour' d1 . neighbour' d2
        iter4 f = f . f . f . f
        center = (0, 0)

northCorner, southCorner, northeastCorner, northwestCorner, southeastCorner, southwestCorner :: Index HalmaGrid
northCorner = corner (Northwest, Northeast)
southCorner = corner (Southwest, Southeast)
northeastCorner = corner (Northeast, East)
northwestCorner = corner (Northwest, West)
southeastCorner = corner (Southeast, East)
southwestCorner = corner (Southwest, West)

corners :: [(Int, Int)]
corners = 
  [ northCorner, southCorner
  , northeastCorner, northwestCorner
  , southeastCorner, southwestCorner
  ]

instance Grid HalmaGrid where
  type Index HalmaGrid = (Int, Int)
  type Direction HalmaGrid = HexDirection
  indices _ = filter (contains HalmaGrid) roughBoard
    where roughBoard = indices (hexHexGrid 9)
  neighbours = neighboursBasedOn UnboundedHexGrid
  distance = distanceBasedOn UnboundedHexGrid
  directionTo = directionToBasedOn UnboundedHexGrid
  contains _ p =
    distCenter <= 4 ||
    (distCenter <= 8 && any ((==) (8 - distCenter) . dist p) corners)
    where dist = distance UnboundedHexGrid
          distCenter = dist (0, 0) p

instance FiniteGrid HalmaGrid where
  type Size HalmaGrid = ()
  size _ = ()
  maxPossibleDistance _ = 16

instance BoundedGrid HalmaGrid where
  tileSideCount _ = 6


data Team = TeamNorth | TeamSouth
          | TeamNortheast | TeamNorthwest
          | TeamSoutheast | TeamSouthwest
          deriving (Eq, Show, Read, Ord, Bounded, Enum, Generic)

oppositeTeam :: Team -> Team
oppositeTeam TeamNorth = TeamSouth
oppositeTeam TeamSouth = TeamNorth
oppositeTeam TeamNortheast = TeamSouthwest
oppositeTeam TeamSouthwest = TeamNortheast
oppositeTeam TeamNorthwest = TeamSoutheast
oppositeTeam TeamSoutheast = TeamNorthwest

startCorner :: Team -> (Int, Int)
startCorner TeamNorth = northCorner
startCorner TeamSouth = southCorner
startCorner TeamNortheast = northeastCorner
startCorner TeamNorthwest = northwestCorner
startCorner TeamSoutheast = southeastCorner
startCorner TeamSouthwest = southwestCorner

endCorner :: Team -> (Int, Int)
endCorner = startCorner . oppositeTeam

startFields, endFields :: Team -> [(Int, Int)]
startFields team = filter ((<= 4) . dist) (indices HalmaGrid)
  where dist = distance HalmaGrid (startCorner team)
endFields = startFields . oppositeTeam

twoPlayers :: Team -> Bool
twoPlayers TeamNorth = True
twoPlayers TeamSouth = True
twoPlayers _ = False

threePlayers :: Team -> Bool
threePlayers TeamNorthwest = True
threePlayers TeamNortheast = True
threePlayers TeamSouth = True
threePlayers _ = False


newtype HalmaBoard = HalmaBoard { toMap :: M.Map (Index HalmaGrid) Team }
  deriving (Eq)

instance Show HalmaBoard where
  show (HalmaBoard m) = "fromMap (" ++ show m ++ ")"

lookupHalmaBoard :: (Int, Int) -> HalmaBoard -> Maybe Team
lookupHalmaBoard p = M.lookup p . toMap

fromMap :: M.Map (Index HalmaGrid) Team -> Maybe HalmaBoard
fromMap m = if invariantsHold then Just (HalmaBoard m) else Nothing
  where invariantsHold = indicesCorrect && rightTeamPieces
        list = M.toList m
        indicesCorrect = all (contains HalmaGrid . fst) list
        rightTeamPieces = all rightNumberOfTeamPieces [minBound..maxBound]
        rightNumberOfTeamPieces team =
          length (filter ((== team) . snd) list) `elem` [0,15]

-- | Move a piece on the halma board. This function does not check whether the
-- move is valid according to the Halma rules.
movePiece
  :: (Int, Int) -- ^ start position
  -> (Int, Int) -- ^ end position
  -> HalmaBoard
  -> Either String HalmaBoard
movePiece startPos endPos (HalmaBoard m) =
  case M.lookup startPos m of
    Nothing -> Left "cannot make move: start position is empty"
    Just team ->
      case M.lookup endPos m of
        Just team' -> Left $ "cannot make move: end position is occupied by team " ++ show team'
        Nothing -> Right $ HalmaBoard $ M.insert endPos team $ M.delete startPos m

initialBoard :: (Team -> Bool) -> HalmaBoard
initialBoard chosenTeams = HalmaBoard (M.fromList lineUps)
  where lineUps = concatMap (\team -> if chosenTeams team then lineUp team else [])
                            [minBound..maxBound]
        lineUp team = map (flip (,) team) (startFields team)

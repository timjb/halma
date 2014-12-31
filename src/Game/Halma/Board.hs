{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Game.Halma.Board
  ( HalmaGridSize (..), HalmaGrid (..)
  , sideLength, numFields
  , HalmaDirection (..)
  , oppositeDirection
  , corner
  , Team
  , startCorner, endCorner
  , startFields, endFields
  , twoPlayers, threePlayers
  , HalmaBoard, getGrid, toMap, fromMap
  , lookupHalmaBoard
  , movePiece
  , initialBoard
  ) where

import GHC.Generics (Generic)
import Math.Geometry.Grid
import Math.Geometry.Grid.Hexagonal
import qualified Math.Geometry.Grid.HexagonalInternal as HI
import Math.Geometry.GridInternal
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

data HalmaGridSize = S | L

data HalmaGrid :: HalmaGridSize -> * where
  SmallGrid :: HalmaGrid 'S
  LargeGrid :: HalmaGrid 'L

instance Eq (HalmaGrid size) where
  _ == _ = True

instance Ord (HalmaGrid size) where
  _ `compare` _ = EQ

instance Show (HalmaGrid size) where
  show SmallGrid = "SmallGrid"
  show LargeGrid = "LargeGrid"

sideLength :: HalmaGrid size -> Int
sideLength SmallGrid = 5
sideLength LargeGrid = 6

numFields :: HalmaGrid size -> Int
numFields SmallGrid = 121
numFields LargeGrid = 181

data HalmaDirection = North | Northeast | Southeast | South | Southwest | Northwest
  deriving (Eq, Show, Read, Ord, Bounded, Enum, Generic)

oppositeDirection :: HalmaDirection -> HalmaDirection
oppositeDirection North = South
oppositeDirection South = North
oppositeDirection Northeast = Southwest
oppositeDirection Southwest = Northeast
oppositeDirection Northwest = Southeast
oppositeDirection Southeast = Northwest

corner :: HalmaGrid size -> HalmaDirection -> (Int, Int)
corner halmaGrid direction = iter (sideLength halmaGrid - 1)
                                  (intoDirection $ getDirs direction) center
  where neighbour' dir = fromJust . flip (neighbour HI.UnboundedHexGrid) dir
        intoDirection (d1, d2) = neighbour' d1 . neighbour' d2
        iter 0 _ = id
        iter i f = iter (i-1) f . f
        center = (0, 0)
        getDirs North = (HI.Northwest, HI.Northeast)
        getDirs South = (HI.Southwest, HI.Southeast)
        getDirs Northeast = (HI.Northeast, HI.East)
        getDirs Northwest = (HI.Northwest, HI.West)
        getDirs Southeast = (HI.Southeast, HI.East)
        getDirs Southwest = (HI.Southwest, HI.West)

instance Grid (HalmaGrid size) where
  type Index (HalmaGrid size) = (Int, Int)
  type Direction (HalmaGrid size) = HI.HexDirection
  -- TODO: cache indices
  indices halmaGrid = filter (contains halmaGrid) roughBoard
    where sl = sideLength halmaGrid - 1
          roughBoard = indices (hexHexGrid (2*sl + 1))
  neighbours = neighboursBasedOn HI.UnboundedHexGrid
  distance = distanceBasedOn HI.UnboundedHexGrid
  directionTo = directionToBasedOn HI.UnboundedHexGrid
  contains halmaGrid p =
    distCenter <= sl ||
    (distCenter <= (2*sl) && any ((==) (2*sl - distCenter) . dist p) corners)
    where sl = sideLength halmaGrid - 1
          dist = distance HI.UnboundedHexGrid
          distCenter = dist (0, 0) p
          corners = map (corner halmaGrid) [minBound..maxBound]

instance FiniteGrid (HalmaGrid S) where
  type Size (HalmaGrid S) = ()
  size _ = ()
  maxPossibleDistance _ = 16

instance FiniteGrid (HalmaGrid L) where
  type Size (HalmaGrid L) = ()
  size _ = ()
  maxPossibleDistance _ = 20

instance BoundedGrid (HalmaGrid size) where
  tileSideCount _ = 6

type Team = HalmaDirection

startCorner :: HalmaGrid size -> Team -> (Int, Int)
startCorner = corner

endCorner :: HalmaGrid size -> Team -> (Int, Int)
endCorner halmaGrid = corner halmaGrid . oppositeDirection

startFields, endFields :: HalmaGrid size -> Team -> [(Int, Int)]
startFields halmaGrid team = filter ((<= 4) . dist) (indices halmaGrid)
  where dist = distance halmaGrid (startCorner halmaGrid team)
endFields halmaGrid = startFields halmaGrid . oppositeDirection

twoPlayers :: Team -> Bool
twoPlayers North = True
twoPlayers South = True
twoPlayers _ = False

threePlayers :: Team -> Bool
threePlayers Northwest = True
threePlayers Northeast = True
threePlayers South = True
threePlayers _ = False


data HalmaBoard size =
       HalmaBoard { getGrid :: HalmaGrid size
                  , toMap :: M.Map (Int, Int) Team
                  } deriving (Eq)

instance Show (HalmaBoard size) where
  show (HalmaBoard halmaGrid m) = "fromMap " ++ show halmaGrid ++ " (" ++ show m ++ ")"

fromMap :: HalmaGrid size -> M.Map (Index (HalmaGrid size)) Team -> Maybe (HalmaBoard size)
fromMap halmaGrid m = if invariantsHold then Just (HalmaBoard halmaGrid m) else Nothing
  where invariantsHold = indicesCorrect && rightTeamPieces
        list = M.toList m
        indicesCorrect = all (contains halmaGrid . fst) list
        rightTeamPieces = all rightNumberOfTeamPieces [minBound..maxBound]
        rightNumberOfTeamPieces team =
          length (filter ((== team) . snd) list) `elem` [0,15]

lookupHalmaBoard :: (Int, Int) -> HalmaBoard size -> Maybe Team
lookupHalmaBoard p = M.lookup p . toMap

-- | Move a piece on the halma board. This function does not check whether the
-- move is valid according to the Halma rules.
movePiece
  :: (Int, Int) -- ^ start position
  -> (Int, Int) -- ^ end position
  -> HalmaBoard size
  -> Either String (HalmaBoard size)
movePiece startPos endPos (HalmaBoard halmaGrid m) =
  case M.lookup startPos m of
    Nothing -> Left "cannot make move: start position is empty"
    Just team ->
      case M.lookup endPos m of
        Just team' -> Left $ "cannot make move: end position is occupied by team " ++ show team'
        Nothing -> Right $ HalmaBoard halmaGrid $ M.insert endPos team $ M.delete startPos m

initialBoard :: HalmaGrid size -> (Team -> Bool) -> HalmaBoard size
initialBoard halmaGrid chosenTeams = HalmaBoard halmaGrid (M.fromList lineUps)
  where lineUps = concatMap (\team -> if chosenTeams team then lineUp team else [])
                            [minBound..maxBound]
        lineUp team = map (flip (,) team) (startFields halmaGrid team)

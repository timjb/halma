{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Halma.Board
  ( HalmaBoard (..)
  , northCorner, southCorner
  , northeastCorner, northwestCorner
  , southeastCorner, southwestCorner
  , corners
  ) where

import GHC.Generics (Generic)
import Math.Geometry.Grid
import Math.Geometry.Grid.Hexagonal
import Math.Geometry.Grid.HexagonalInternal (HexDirection (..), UnboundedHexGrid (..))
import Math.Geometry.GridInternal
import Data.Maybe (fromJust)

data HalmaBoard = HalmaBoard
  deriving (Eq, Show, Read, Ord, Bounded, Generic)

corner :: (HexDirection, HexDirection) -> (Int, Int)
corner d = iter4 (intoDirection d) center
  where neighbour' d = fromJust . flip (neighbour UnboundedHexGrid) d
        intoDirection (d1, d2) = neighbour' d1 . neighbour' d2
        iter4 f = f . f . f . f
        center = (0, 0)

northCorner, southCorner, northeastCorner, northwestCorner, southeastCorner, southwestCorner :: Index HalmaBoard
northCorner = corner (Northwest, Northeast)
southCorner = corner (Southwest, Southeast)
northeastCorner = corner (Northeast, East)
northwestCorner = corner (Northwest, West)
southeastCorner = corner (Southeast, East)
southwestCorner = corner (Southwest, West)

corners :: [Index HalmaBoard]
corners = 
  [ northCorner, southCorner
  , northeastCorner, northwestCorner
  , southeastCorner, southwestCorner
  ]

instance Grid HalmaBoard where
  type Index HalmaBoard = (Int, Int)
  type Direction HalmaBoard = HexDirection
  indices _ = filter (contains HalmaBoard) roughBoard
    where roughBoard = indices (hexHexGrid 9)
  neighbours = neighboursBasedOn UnboundedHexGrid
  distance = distanceBasedOn UnboundedHexGrid
  directionTo = directionToBasedOn UnboundedHexGrid
  contains _ p =
    distCenter <= 4 ||
    (distCenter <= 8 && any ((==) (8 - distCenter) . dist p) corners)
    where dist = distance UnboundedHexGrid
          distCenter = dist (0, 0) p

instance FiniteGrid HalmaBoard where
  type Size HalmaBoard = ()
  size _ = ()
  maxPossibleDistance _ = 16

instance BoundedGrid HalmaBoard where
  tileSideCount _ = 6
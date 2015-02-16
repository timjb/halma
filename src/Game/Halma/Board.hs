{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Game.Halma.Board
  ( HalmaGridSize (..), HalmaGrid (..)
  , sideLength, numberOfFields
  , HalmaDirection (..)
  , oppositeDirection
  , rowsInDirection
  , corner
  , Team
  , startCorner, endCorner
  , startFields, endFields
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

-- | Numbers of fields on each straight edge of a star-shaped halma board of the
-- given size.
sideLength :: HalmaGrid size -> Int
sideLength SmallGrid = 5
sideLength LargeGrid = 6

-- | Total number of fields on a halma board of the given size.
numberOfFields :: HalmaGrid size -> Int
numberOfFields SmallGrid = 121
numberOfFields LargeGrid = 181

-- | The six corners of a star-shaped halma board.
data HalmaDirection = North | Northeast | Southeast | South | Southwest | Northwest
  deriving (Eq, Show, Read, Ord, Bounded, Enum, Generic)

oppositeDirection :: HalmaDirection -> HalmaDirection
oppositeDirection North = South
oppositeDirection South = North
oppositeDirection Northeast = Southwest
oppositeDirection Southwest = Northeast
oppositeDirection Northwest = Southeast
oppositeDirection Southeast = Northwest

getDirs :: HalmaDirection -> (HI.HexDirection, HI.HexDirection)
getDirs North = (HI.Northwest, HI.Northeast)
getDirs South = (HI.Southwest, HI.Southeast)
getDirs Northeast = (HI.Northeast, HI.East)
getDirs Northwest = (HI.Northwest, HI.West)
getDirs Southeast = (HI.Southeast, HI.East)
getDirs Southwest = (HI.Southwest, HI.West)

neighbour' :: HI.HexDirection -> (Int, Int) -> (Int, Int)
neighbour' dir = fromJust . flip (neighbour HI.UnboundedHexGrid) dir

-- | From the point of view of the given corner: On which row lies the given
-- field? The row through the center is row zero, rows nearer to the corner have
-- positive, rows nearer to the opposite corner negative numbers.
rowsInDirection :: HalmaDirection -> (Int, Int) -> Int
rowsInDirection dir = cramerPlus (neighbour' dir1 (0,0)) (neighbour' dir2 (0,0))
  where (dir1, dir2) = getDirs dir
        cramerPlus (a,b) (c,d) (x,y) =
          -- Computes (e+f) where (e,f) is the solution of M*(e,f) = (x,y)
          -- where M is the matrix with column vectors (a,b) and (c,d).
          -- Precondition: det(M) = 1/det(M), i.e. det(M) `elem` [-1,1].
          let det = a*d - b*c
          in det * (x*(d-b) + y*(a-c))
        

-- | The corner corresponding to a direction on a star-shaped board of the
-- given size.
corner :: HalmaGrid size -> HalmaDirection -> (Int, Int)
corner halmaGrid direction = (sl*x, sl*y)
  where (d1, d2) = getDirs direction
        sl = sideLength halmaGrid - 1
        (x, y) = neighbour' d1 $ neighbour' d2 (0, 0)

instance Grid (HalmaGrid size) where
  type Index (HalmaGrid size) = (Int, Int)
  type Direction (HalmaGrid size) = HI.HexDirection
  indices halmaGrid = filter (contains halmaGrid) roughBoard
    where sl = sideLength halmaGrid - 1
          roughBoard = indices (hexHexGrid (2*sl + 1))
  neighbours = neighboursBasedOn HI.UnboundedHexGrid
  distance = distanceBasedOn HI.UnboundedHexGrid
  directionTo = directionToBasedOn HI.UnboundedHexGrid
  contains halmaGrid (x, y) = atLeastTwo (test x) (test y) (test z)
    where z = x + y
          test i = abs i <= sl
          sl = sideLength halmaGrid - 1
          atLeastTwo True True _ = True
          atLeastTwo True False True = True
          atLeastTwo False True True = True
          atLeastTwo _ _ _ = False

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

-- | The corner where the team starts.
type Team = HalmaDirection

-- | The position of the corner field where a team starts.
startCorner :: HalmaGrid size -> Team -> (Int, Int)
startCorner = corner

-- | The position of the end zone corner of a team.
endCorner :: HalmaGrid size -> Team -> (Int, Int)
endCorner halmaGrid = corner halmaGrid . oppositeDirection

-- | The start positions of a team's pieces.
startFields :: HalmaGrid size -> Team -> [(Int, Int)]
startFields halmaGrid team = filter ((<= 4) . dist) (indices halmaGrid)
  where dist = distance halmaGrid (startCorner halmaGrid team)

-- | The end zone of the given team.
endFields :: HalmaGrid size -> Team -> [(Int, Int)]
endFields halmaGrid = startFields halmaGrid . oppositeDirection

-- | Map from board positions to the team occupying that position.
data HalmaBoard size =
       HalmaBoard { getGrid :: HalmaGrid size
                  , toMap :: M.Map (Int, Int) Team
                  } deriving (Eq)

instance Show (HalmaBoard size) where
  show (HalmaBoard halmaGrid m) = "fromMap " ++ show halmaGrid ++ " (" ++ show m ++ ")"

-- | Construct halma boards. Satisfies @fromMap (getGrid board) (toMap board) = Just board@.
fromMap :: HalmaGrid size -> M.Map (Index (HalmaGrid size)) Team -> Maybe (HalmaBoard size)
fromMap halmaGrid m = if invariantsHold then Just (HalmaBoard halmaGrid m) else Nothing
  where invariantsHold = indicesCorrect && rightTeamPieces
        list = M.toList m
        indicesCorrect = all (contains halmaGrid . fst) list
        rightTeamPieces = all rightNumberOfTeamPieces [minBound..maxBound]
        rightNumberOfTeamPieces team =
          length (filter ((== team) . snd) list) `elem` [0,15]

-- | Lookup whether a position on the board is occupied, and 
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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Halma.Board
  ( HalmaGrid (..)
  , sideLength, numberOfFields
  , HalmaDirection (..)
  , oppositeDirection
  , leftOf
  , rowsInDirection
  , corner
  , Team
  , startCorner, endCorner
  , startFields, endFields
  , Piece (..)
  , HalmaBoard, getGrid, toMap, fromMap
  , lookupHalmaBoard
  , Move (..)
  , movePiece
  , initialBoard
  ) where

import Control.Monad (unless)
import Data.Aeson ((.=), (.:))
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Math.Geometry.Grid as Grid
import qualified Math.Geometry.GridInternal as Grid
import qualified Math.Geometry.Grid.Hexagonal as HexGrid
import qualified Math.Geometry.Grid.HexagonalInternal as HexGrid

data HalmaGrid
  = SmallGrid
  | LargeGrid
  deriving (Eq, Show, Ord)

instance A.ToJSON HalmaGrid where
  toJSON grid =
    case grid of
      SmallGrid -> "SmallGrid"
      LargeGrid -> "LargeGrid"

instance A.FromJSON HalmaGrid where
  parseJSON =
    A.withText "HalmaGrid" $ \case
      "SmallGrid" -> pure SmallGrid
      "LargeGrid" -> pure LargeGrid
      _other -> fail "expected string 'SmallGrid' or 'LargeGrid'"

-- | Numbers of fields on each straight edge of a star-shaped halma board of the
-- given size.
sideLength :: HalmaGrid -> Int
sideLength grid =
  case grid of
    SmallGrid -> 5
    LargeGrid -> 6

-- | Total number of fields on a halma board of the given size.
numberOfFields :: HalmaGrid -> Int
numberOfFields grid =
  case grid of
    SmallGrid -> 121
    LargeGrid -> 181

-- | The six corners of a star-shaped halma board.
data HalmaDirection
  = North
  | Northeast
  | Southeast
  | South
  | Southwest
  | Northwest
  deriving (Eq, Show, Read, Ord, Bounded, Enum, Generic)

instance A.ToJSON HalmaDirection where
  toJSON dir =
    case dir of
      North -> "N"
      Northeast -> "NE"
      Southeast -> "SE"
      South -> "S"
      Southwest -> "SW"
      Northwest -> "NW"

instance A.FromJSON HalmaDirection where
  parseJSON =
    A.withText "HalmaDirection" $ \text ->
      case text of
        "N"  -> pure North
        "NE" -> pure Northeast
        "SE" -> pure Southeast
        "S"  -> pure South
        "SW" -> pure Southwest
        "NW" -> pure Northwest
        _    -> fail "expected a Halma direction (one of N, NE, SE, S, SW, NW)"

oppositeDirection :: HalmaDirection -> HalmaDirection
oppositeDirection dir =
  case dir of
    North -> South
    South -> North
    Northeast -> Southwest
    Southwest -> Northeast
    Northwest -> Southeast
    Southeast -> Northwest

leftOf :: HalmaDirection -> HalmaDirection
leftOf dir =
  case dir of
    North -> Northwest
    Northwest -> Southwest
    Southwest -> South
    South -> Southeast
    Southeast -> Northeast
    Northeast -> North

getDirs :: HalmaDirection -> (HexGrid.HexDirection, HexGrid.HexDirection)
getDirs dir =
  case dir of
    North -> (HexGrid.Northwest, HexGrid.Northeast)
    South -> (HexGrid.Southwest, HexGrid.Southeast)
    Northeast -> (HexGrid.Northeast, HexGrid.East)
    Northwest -> (HexGrid.Northwest, HexGrid.West)
    Southeast -> (HexGrid.Southeast, HexGrid.East)
    Southwest -> (HexGrid.Southwest, HexGrid.West)

neighbour' :: HexGrid.HexDirection -> (Int, Int) -> (Int, Int)
neighbour' dir = fromJust . flip (Grid.neighbour HexGrid.UnboundedHexGrid) dir

-- | From the point of view of the given corner: On which row lies the given
-- field? The row through the center is row zero, rows nearer to the corner have
-- positive, rows nearer to the opposite corner negative numbers.
rowsInDirection :: HalmaDirection -> (Int, Int) -> Int
rowsInDirection dir =
  cramerPlus (neighbour' dir1 (0,0)) (neighbour' dir2 (0,0))
  where
    (dir1, dir2) = getDirs dir
    cramerPlus (a,b) (c,d) (x,y) =
      -- Computes (e+f) where (e,f) is the solution of M*(e,f) = (x,y)
      -- where M is the matrix with column vectors (a,b) and (c,d).
      -- Precondition: det(M) = 1/det(M), i.e. det(M) `elem` [-1,1].
      let det = a*d - b*c
      in det * (x*(d-b) + y*(a-c))

-- | The corner corresponding to a direction on a star-shaped board of the
-- given size.
corner :: HalmaGrid -> HalmaDirection -> (Int, Int)
corner halmaGrid direction = (sl*x, sl*y)
  where
    (d1, d2) = getDirs direction
    sl = sideLength halmaGrid - 1
    (x, y) = neighbour' d1 $ neighbour' d2 (0, 0)

instance Grid.Grid HalmaGrid where
  type Index HalmaGrid = (Int, Int)
  type Direction HalmaGrid = HexGrid.HexDirection
  indices halmaGrid =
    filter (Grid.contains halmaGrid) roughBoard
    where
      sl = sideLength halmaGrid - 1
      roughBoard = Grid.indices (HexGrid.hexHexGrid (2*sl + 1))
  neighbours = Grid.neighboursBasedOn HexGrid.UnboundedHexGrid
  distance = Grid.distanceBasedOn HexGrid.UnboundedHexGrid
  directionTo = Grid.directionToBasedOn HexGrid.UnboundedHexGrid
  contains halmaGrid (x, y) = atLeastTwo (test x) (test y) (test z)
    where
      z = x + y
      test i = abs i <= sl
      sl = sideLength halmaGrid - 1
      atLeastTwo True True _ = True
      atLeastTwo True False True = True
      atLeastTwo False True True = True
      atLeastTwo _ _ _ = False

instance Grid.FiniteGrid HalmaGrid where
  type Size HalmaGrid = HalmaGrid
  size = id
  maxPossibleDistance = \case
    SmallGrid -> 16
    LargeGrid -> 20

instance Grid.BoundedGrid HalmaGrid where
  tileSideCount _ = 6

-- | The corner where the team starts.
type Team = HalmaDirection

-- | The position of the corner field where a team starts.
startCorner :: HalmaGrid -> Team -> (Int, Int)
startCorner = corner

-- | The position of the end zone corner of a team.
endCorner :: HalmaGrid -> Team -> (Int, Int)
endCorner halmaGrid = corner halmaGrid . oppositeDirection

-- | The start positions of a team's pieces.
startFields :: HalmaGrid -> Team -> [(Int, Int)]
startFields halmaGrid team = filter ((<= 4) . dist) (Grid.indices halmaGrid)
  where dist = Grid.distance halmaGrid (startCorner halmaGrid team)

-- | The end zone of the given team.
endFields :: HalmaGrid -> Team -> [(Int, Int)]
endFields halmaGrid = startFields halmaGrid . oppositeDirection

-- | Halma gaming piece
data Piece
  = Piece
  { pieceTeam :: Team  -- ^ player
  , pieceNumber :: Word8 -- ^ number between 1 and 15
  } deriving (Show, Eq, Ord)

instance A.ToJSON Piece where
  toJSON piece =
    A.object
      [ "team" .= A.toJSON (pieceTeam piece)
      , "number" .= A.toJSON (pieceNumber piece)
      ]

instance A.FromJSON Piece where
  parseJSON =
    A.withObject "Piece" $ \o -> do
      team <- o .: "team"
      number <- o .: "number"
      unless (1 <= number && number <= 15) $
        fail "pieces must have a number between 1 and 15!"
      pure Piece { pieceTeam = team, pieceNumber = number }

-- | Map from board positions to the team occupying that position.
data HalmaBoard =
  HalmaBoard
  { getGrid :: HalmaGrid
  , toMap :: M.Map (Int, Int) Piece
  } deriving (Eq, Show)

instance A.ToJSON HalmaBoard where
  toJSON board =
    A.object
      [ "grid" .= A.toJSON (getGrid board)
      , "occupied_fields" .= map fieldToJSON (M.assocs (toMap board))
      ]
    where
      fieldToJSON ((x, y), piece) =
        A.object
          [ "x" .= x
          , "y" .= y
          , "piece" .= A.toJSON piece
          ]

instance A.FromJSON HalmaBoard where
  parseJSON =
    A.withObject "HalmaGrid size" $ \o -> do
      grid <- o .: "grid"
      fieldVals <- o .: "occupied_fields"
      fieldsMap <- M.fromList <$> mapM parseFieldFromJSON fieldVals
      case fromMap grid fieldsMap of
        Just board -> pure board
        Nothing ->
          fail "the JSON describing the occupied fields violates some invariant!"
    where
      parseFieldFromJSON =
        A.withObject "field" $ \o -> do
          x <- o .: "x"
          y <- o .: "y"
          piece <- o .: "piece"
          pure ((x, y), piece)

-- | Construct halma boards. Satisfies
-- @fromMap (getGrid board) (toMap board) = Just board@.
fromMap
  :: HalmaGrid
  -> M.Map (Grid.Index HalmaGrid) Piece
  -> Maybe HalmaBoard
fromMap halmaGrid m =
  if invariantsHold then
    Just (HalmaBoard halmaGrid m)
  else
    Nothing
  where
    invariantsHold = indicesCorrect && rightTeamPieces
    list = M.toList m
    indicesCorrect = all (Grid.contains halmaGrid . fst) list
    allTeams = [minBound..maxBound]
    rightTeamPieces = all rightNumberOfTeamPieces allTeams
    rightNumberOfTeamPieces team =
      let teamPieces = filter ((== team) . pieceTeam) (map snd list)
      in null teamPieces || sort teamPieces == map (Piece team) [1..15]

-- | Lookup whether a position on the board is occupied, and
lookupHalmaBoard :: (Int, Int) -> HalmaBoard -> Maybe Piece
lookupHalmaBoard p = M.lookup p . toMap

-- | A move of piece on a (Halma) board.
data Move
  = Move
  { moveFrom :: (Int, Int) -- ^ start position
  , moveTo :: (Int, Int) -- ^ end position, must be different from start position
  } deriving (Show, Eq)

instance A.ToJSON Move where
  toJSON move =
    A.object
      [ "from" .= coordsToJSON (moveFrom move)
      , "to"   .= coordsToJSON (moveTo move)
      ]
    where
      coordsToJSON (x, y) = A.object [ "x" .= x, "y" .= y ]

instance A.FromJSON Move where
  parseJSON =
    A.withObject "Move" $ \o -> do
      from <- parseCoordsFromJSON =<< o .: "from"
      to <- parseCoordsFromJSON =<< o .: "to"
      pure Move { moveFrom = from, moveTo = to }
    where
      parseCoordsFromJSON =
        A.withObject "(Int, Int)" $ \o ->
          (,) <$> o .: "x" <*> o .: "y"

-- | Move a piece on the halma board. This function does not check whether the
-- move is valid according to the Halma rules.
movePiece
  :: Move
  -> HalmaBoard
  -> Either String HalmaBoard
movePiece Move { moveFrom = startPos, moveTo = endPos } (HalmaBoard halmaGrid m) =
  case M.lookup startPos m of
    Nothing -> Left "cannot make move: start position is empty"
    Just piece ->
      case M.lookup endPos m of
        Just otherPiece ->
          Left $
            "cannot make move: end position is occupied by team " ++
            show (pieceTeam otherPiece)
        Nothing ->
          let m' = M.insert endPos piece (M.delete startPos m)
          in Right (HalmaBoard halmaGrid m')

initialBoard :: HalmaGrid -> (Team -> Bool) -> HalmaBoard
initialBoard halmaGrid chosenTeams = HalmaBoard halmaGrid (M.fromList lineUps)
  where
    allTeams = [minBound..maxBound]
    lineUps = concatMap lineUp allTeams
    mkPiece team number position =
      (position, Piece { pieceTeam = team, pieceNumber = number })
    lineUp team =
      if chosenTeams team
      then zipWith (mkPiece team) [1..15] (startFields halmaGrid team)
      else []

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Game.Halma.Board.Tests (tests) where

import Control.Monad (forM_)
import Data.List (permutations, sortBy)
import Data.Maybe (isJust, fromJust)
import Data.Function (on)
import Game.Halma.Board
import Math.Geometry.Grid
import Test.HUnit hiding (Test)
import Test.QuickCheck hiding (Result)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.Map.Strict as M
import qualified Math.Geometry.Grid.HexagonalInternal as HexGrid

testRowsInDirection :: Assertion
testRowsInDirection =
  forM_ [minBound..maxBound] $ \halmaDir -> do
    let fieldsAtRow i = filter ((== i) . rowsInDirection halmaDir) (indices SmallGrid)
        expected = let xs = [0,1,2,3,4,13,12,11,10] in xs ++ [9] ++ reverse xs
    expected @=? map (length . fieldsAtRow) [-9..9]
    8    @=? rowsInDirection halmaDir (corner SmallGrid halmaDir)
    (-8) @=? rowsInDirection halmaDir (corner SmallGrid (oppositeDirection halmaDir))

-- | Cutoff equality comparison
(@=?*) :: (Eq a, Show a) => [a] -> [a] -> Assertion
expected @=?* actual = expected @=? take (length expected) actual

testDistancesFromCenter :: Assertion
testDistancesFromCenter = do
  [1,6,12,18,24,24,18,12,6,0,0,0] @=?* actual SmallGrid
  [1,6,12,18,24,30,30,24,18,12,6,0,0,0] @=?* actual LargeGrid
  where
    distCenter :: HalmaGrid -> (Int, Int) -> Int
    distCenter hg = distance hg (0, 0)
    fieldsAtDistance :: HalmaGrid -> Int -> [(Int, Int)]
    fieldsAtDistance hg d = filter ((== d) . distCenter hg) (indices hg)
    actual :: HalmaGrid -> [Int]
    actual hg = map (length . fieldsAtDistance hg) [0,1..]

testBoundaryLength :: Assertion
testBoundaryLength = do
  48 @=? length (boundary SmallGrid)
  60 @=? length (boundary LargeGrid)

testDirectionTo :: Assertion
testDirectionTo = do
  let c = corner SmallGrid
  [HexGrid.Northeast] @=? directionTo SmallGrid (c South) (c Southeast)
  [HexGrid.Northeast] @=? directionTo SmallGrid (c South) (c Northeast)
  assertOneOf
    (directionTo SmallGrid (c South) (c North))
    (permutations [HexGrid.Northeast, HexGrid.Northwest])

assertOneOf :: (Eq a, Show a) => a -> [a] -> Assertion
assertOneOf actual validResults = assertBool msg (actual `elem` validResults)
  where msg = "Expected '" ++ show actual ++ "' to be one of '" ++ show validResults ++ "'"

numbersCorrect :: HalmaGrid -> [Maybe Piece] -> Bool
numbersCorrect grid = go 15 15 (numberOfFields grid - 2*15)
  where
    go :: Int -> Int -> Int -> [Maybe Piece] -> Bool
    go 0 0 0 [] = True
    go _ _ _ [] = False
    go !n !s !e (piece : pieces) =
      case pieceTeam <$> piece of
        Just North -> go (n-1) s e pieces
        Just South -> go n (s-1) e pieces
        Just _otherTeam -> False
        Nothing -> go n s (e-1) pieces

testInitialBoard :: Assertion
testInitialBoard = ass SmallGrid >> ass LargeGrid
  where
    ass hg = assertBool "Expected 15 pieces of team north and south" $ numbersCorrect hg (pieces hg)
    pieces hg = map (\p -> lookupHalmaBoard p (initialBoard hg twoPlayers)) (indices hg)
    twoPlayers :: Team -> Bool
    twoPlayers team = team `elem` [North, South]

arbitraryPerm :: [a] -> Gen [a]
arbitraryPerm xs =
  fmap (map fst . sortBy (compare `on` snd) . zip xs)
       (vectorOf (length xs) arbitrary :: Gen [Double])

genBoard :: HalmaGrid -> Gen HalmaBoard
genBoard grid = do
  pieces <- arbitraryPerm (indices grid)
  let (northTeam, restPieces) = splitAt 15 pieces
      southTeam = take 15 restPieces
  return $ fromJust $ fromMap grid $ M.fromList $
    mkPieces North northTeam ++
    mkPieces South southTeam
  where
    mkPieces team positions =
      let mkPiece pos ix = (pos, Piece { pieceNumber = ix, pieceTeam = team})
      in zipWith mkPiece positions [1..15]

instance Arbitrary HalmaGrid where
  arbitrary = elements [SmallGrid, LargeGrid]

instance Arbitrary HalmaBoard where
  arbitrary = do
    grid <- arbitrary
    genBoard grid

genHalmaGridPos :: HalmaGrid -> Gen (Int, Int)
genHalmaGridPos grid = elements (indices grid)

prop_fromMap :: HalmaBoard -> Bool
prop_fromMap halmaBoard = fromMap (getGrid halmaBoard) (toMap halmaBoard) == Just halmaBoard

prop_moveNumbersInvariant :: HalmaBoard -> Gen Bool
prop_moveNumbersInvariant board = do
  let grid = getGrid board
  startPos <- genHalmaGridPos grid
  endPos <- genHalmaGridPos grid
  let move = Move { moveFrom = startPos, moveTo = endPos }
  case movePiece move board of
    Left _err ->
      -- may only fail when there is no piece on the start position or a piece
      -- on the end position
      return $ lookupHalmaBoard startPos board == Nothing || isJust (lookupHalmaBoard endPos board)
    Right board' -> do
      let pieces = map (\p -> lookupHalmaBoard p board') (indices grid)
      return $
        lookupHalmaBoard endPos board' == lookupHalmaBoard startPos board &&
        lookupHalmaBoard endPos board  == lookupHalmaBoard startPos board' &&
        numbersCorrect grid pieces

tests :: Test
tests =
  testGroup "Game.Halma.Board.Tests"
  [ testCase "testRowsInDirection" testRowsInDirection
  , testCase "testDistancesFromCenter" testDistancesFromCenter
  , testCase "testBoundaryLength" testBoundaryLength
  , testCase "testDirectionTo" testDirectionTo
  , testCase "testInitialBoard" testInitialBoard
  , testProperty "prop_fromMap" prop_fromMap
  , testProperty "prop_moveNumbersInvariant" prop_moveNumbersInvariant
  ]

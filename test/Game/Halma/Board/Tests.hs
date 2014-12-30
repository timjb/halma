{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Game.Halma.Board.Tests (tests) where

import Test.HUnit hiding (Test)
import Test.QuickCheck hiding (Result)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.List (permutations, sortBy)
import Data.Maybe (isJust, fromJust)
import Data.Function (on)
import qualified Data.Map.Strict as M

import Math.Geometry.Grid
import Math.Geometry.Grid.HexagonalInternal (HexDirection (..))
import Game.Halma.Board

testDistancesFromCenter :: Assertion
testDistancesFromCenter = expected @=? actual
  where center = (0, 0)
        distCenter = distance HalmaGrid center
        expected = [1, 6, 12, 18, 24, 24, 18, 12, 6, 0, 0, 0]
        fieldsAtDistance d = filter ((== d) . distCenter) (indices HalmaGrid)
        actual = take (length expected) $ map (length . fieldsAtDistance) [0,1..]

testBoundaryLength :: Assertion
testBoundaryLength = 48 @=? actual
  where actual = length (boundary HalmaGrid)

testDirectionTo :: Assertion
testDirectionTo = do
  [Northeast] @=? directionTo HalmaGrid southCorner southeastCorner
  [Northeast] @=? directionTo HalmaGrid southCorner northeastCorner
  assertOneOf
    (directionTo HalmaGrid southCorner northCorner)
    (permutations [Northeast, Northwest])

assertOneOf :: (Eq a, Show a) => a -> [a] -> Assertion
assertOneOf actual validResults = assertBool msg (actual `elem` validResults)
  where msg = "Expected '" ++ show actual ++ "' to be one of '" ++ show validResults ++ "'"

numbersCorrect :: [Maybe Team] -> Bool
numbersCorrect = go 15 15 (121 - 2*15)
  where go :: Int -> Int -> Int -> [Maybe Team] -> Bool
        go 0 0 0 [] = True
        go !n !s !e (Just TeamNorth : rs) = go (n-1) s e rs
        go !n !s !e (Just TeamSouth : rs) = go n (s-1) e rs
        go !n !s !e (Nothing : rs) = go n s (e-1) rs
        go _ _ _ _ = False

testInitialBoard :: Assertion
testInitialBoard = assertBool "Expected 15 pieces of team north and south" (numbersCorrect pieces)
  where pieces = map (\p -> lookupHalmaBoard p (initialBoard twoPlayers)) (indices HalmaGrid)

arbitraryPerm :: [a] -> Gen [a]
arbitraryPerm xs =
  fmap (map fst . sortBy (compare `on` snd) . zip xs)
       (vectorOf (length xs) arbitrary :: Gen [Double])

instance Arbitrary HalmaBoard where
  arbitrary = do
    pieces <- arbitraryPerm (indices HalmaGrid)
    let (northTeam, restPieces) = splitAt 15 pieces
        southTeam = take 15 restPieces
    return $ fromJust $ fromMap $ M.fromList $
      map (flip (,) TeamNorth) northTeam ++
      map (flip (,) TeamSouth) southTeam

genHalmaGridPos :: Gen (Int, Int)
genHalmaGridPos = elements (indices HalmaGrid)

prop_fromMap :: HalmaBoard -> Bool
prop_fromMap halmaBoard = fromMap (toMap halmaBoard) == Just halmaBoard

prop_moveNumbersInvariant :: HalmaBoard -> Gen Bool
prop_moveNumbersInvariant halmaBoard = do
  startPos <- genHalmaGridPos
  endPos <- genHalmaGridPos
  case movePiece startPos endPos halmaBoard of
    Left _err ->
      -- may only fail when there is no piece on the start position or a piece
      -- on the end position
      return $ lookupHalmaBoard startPos halmaBoard == Nothing || isJust (lookupHalmaBoard endPos halmaBoard)
    Right halmaBoard' -> do
      let pieces = map (\p -> lookupHalmaBoard p (initialBoard twoPlayers)) (indices HalmaGrid)
      return $ lookupHalmaBoard endPos halmaBoard' == lookupHalmaBoard startPos halmaBoard &&
               lookupHalmaBoard endPos halmaBoard  == lookupHalmaBoard startPos halmaBoard' &&
               numbersCorrect pieces


tests :: Test
tests = testGroup "Game.Halma.Board.Tests"
  [ testCase "testDistancesFromCenter" testDistancesFromCenter
  , testCase "testBoundaryLength" testBoundaryLength
  , testCase "testDirectionTo" testDirectionTo
  , testCase "testInitialBoard" testInitialBoard
  , testProperty "fromMap" prop_fromMap
  , testProperty "prop_moveNumbersInvariant" prop_moveNumbersInvariant
  ]

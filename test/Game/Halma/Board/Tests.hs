{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE GADTs #-}

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
import qualified Math.Geometry.Grid.HexagonalInternal as HI
import Game.Halma.Board

-- | Cutoff equality comparison
(@=?*) :: (Eq a, Show a) => [a] -> [a] -> Assertion
expected @=?* actual = expected @=? take (length expected) actual

testDistancesFromCenter :: Assertion
testDistancesFromCenter = do
  [1,6,12,18,24,24,18,12,6,0,0,0] @=?* actual SmallGrid
  [1,6,12,18,24,30,30,24,18,12,6,0,0,0] @=?* actual LargeGrid
  where center = (0, 0)
        distCenter hg = distance hg center
        fieldsAtDistance hg d = filter ((== d) . distCenter hg) (indices hg)
        actual hg = map (length . fieldsAtDistance hg) [0,1..]

testBoundaryLength :: Assertion
testBoundaryLength = do
  48 @=? length (boundary SmallGrid)
  60 @=? length (boundary LargeGrid)

testDirectionTo :: Assertion
testDirectionTo = do
  let c = corner SmallGrid
  [HI.Northeast] @=? directionTo SmallGrid (c South) (c Southeast)
  [HI.Northeast] @=? directionTo SmallGrid (c South) (c Northeast)
  assertOneOf
    (directionTo SmallGrid (c South) (c North))
    (permutations [HI.Northeast, HI.Northwest])

assertOneOf :: (Eq a, Show a) => a -> [a] -> Assertion
assertOneOf actual validResults = assertBool msg (actual `elem` validResults)
  where msg = "Expected '" ++ show actual ++ "' to be one of '" ++ show validResults ++ "'"

numbersCorrect :: HalmaGrid size -> [Maybe Team] -> Bool
numbersCorrect halmaGrid = go 15 15 (numFields halmaGrid - 2*15)
  where go :: Int -> Int -> Int -> [Maybe Team] -> Bool
        go 0 0 0 [] = True
        go !n !s !e (Just North : rs) = go (n-1) s e rs
        go !n !s !e (Just South : rs) = go n (s-1) e rs
        go !n !s !e (Nothing : rs) = go n s (e-1) rs
        go _ _ _ _ = False

testInitialBoard :: Assertion
testInitialBoard = ass SmallGrid >> ass LargeGrid
  where ass hg = assertBool "Expected 15 pieces of team north and south" $ numbersCorrect hg (pieces hg)
        pieces hg = map (\p -> lookupHalmaBoard p (initialBoard hg twoPlayers)) (indices hg)

arbitraryPerm :: [a] -> Gen [a]
arbitraryPerm xs =
  fmap (map fst . sortBy (compare `on` snd) . zip xs)
       (vectorOf (length xs) arbitrary :: Gen [Double])

genBoard :: HalmaGrid size -> Gen (HalmaBoard size)
genBoard halmaGrid = do
  pieces <- arbitraryPerm (indices halmaGrid)
  let (northTeam, restPieces) = splitAt 15 pieces
      southTeam = take 15 restPieces
  return $ fromJust $ fromMap halmaGrid $ M.fromList $
    map (flip (,) North) northTeam ++
    map (flip (,) South) southTeam

instance Arbitrary (HalmaBoard 'S) where
  arbitrary = genBoard SmallGrid

instance Arbitrary (HalmaBoard 'L) where
  arbitrary = genBoard LargeGrid

genHalmaGridPos :: HalmaGrid size -> Gen (Int, Int)
genHalmaGridPos halmaGrid = elements (indices halmaGrid)

prop_fromMap :: HalmaBoard size -> Bool
prop_fromMap halmaBoard = fromMap (getGrid halmaBoard) (toMap halmaBoard) == Just halmaBoard

prop_moveNumbersInvariant :: HalmaBoard size -> Gen Bool
prop_moveNumbersInvariant halmaBoard = do
  let halmaGrid = getGrid halmaBoard
  startPos <- genHalmaGridPos halmaGrid
  endPos <- genHalmaGridPos halmaGrid
  case movePiece startPos endPos halmaBoard of
    Left _err ->
      -- may only fail when there is no piece on the start position or a piece
      -- on the end position
      return $ lookupHalmaBoard startPos halmaBoard == Nothing || isJust (lookupHalmaBoard endPos halmaBoard)
    Right halmaBoard' -> do
      let pieces = map (\p -> lookupHalmaBoard p halmaBoard') (indices halmaGrid)
      return $ lookupHalmaBoard endPos halmaBoard' == lookupHalmaBoard startPos halmaBoard &&
               lookupHalmaBoard endPos halmaBoard  == lookupHalmaBoard startPos halmaBoard' &&
               numbersCorrect halmaGrid pieces


tests :: Test
tests = testGroup "Game.Halma.Board.Tests"
  [ testCase "testDistancesFromCenter" testDistancesFromCenter
  , testCase "testBoundaryLength" testBoundaryLength
  , testCase "testDirectionTo" testDirectionTo
  , testCase "testInitialBoard" testInitialBoard
  , testProperty "prop_fromMap(S)" (prop_fromMap :: HalmaBoard 'S -> Bool)
  , testProperty "prop_fromMap(L)" (prop_fromMap :: HalmaBoard 'L -> Bool)
  , testProperty "prop_moveNumbersInvariant(S)" (prop_moveNumbersInvariant :: HalmaBoard 'S -> Gen Bool)
  , testProperty "prop_moveNumbersInvariant(L)" (prop_moveNumbersInvariant :: HalmaBoard 'L -> Gen Bool)
  ]

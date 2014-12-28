module Game.Halma.Board.Tests (tests) where

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Data.List (permutations)

import Math.Geometry.Grid
import Math.Geometry.Grid.HexagonalInternal (HexDirection (..))
import Game.Halma.Board

testDistancesFromCenter :: Assertion
testDistancesFromCenter = expected @=? actual
  where center = (0, 0)
        distCenter = distance HalmaBoard center
        expected = [1, 6, 12, 18, 24, 24, 18, 12, 6, 0, 0, 0]
        fieldsAtDistance d = filter ((== d) . distCenter) (indices HalmaBoard)
        actual = take (length expected) $ map (length . fieldsAtDistance) [0,1..]

testBoundaryLength :: Assertion
testBoundaryLength = 48 @=? actual
  where actual = length (boundary HalmaBoard)

testDirectionTo :: Assertion
testDirectionTo = do
  let center = (0, 0)
  [Northeast] @=? directionTo HalmaBoard southCorner southeastCorner
  [Northeast] @=? directionTo HalmaBoard southCorner northeastCorner
  assertOneOf
    (directionTo HalmaBoard southCorner northCorner)
    (permutations [Northeast, Northwest])

assertOneOf :: (Eq a, Show a) => a -> [a] -> Assertion
assertOneOf actual validResults = assertBool msg (actual `elem` validResults)
  where msg = "Expected '" ++ show actual ++ "' to be one of '" ++ show validResults ++ "'"

tests :: Test
tests = testGroup "Game.Halma.Board.Tests"
  [ testCase "testDistancesFromCenter" testDistancesFromCenter
  , testCase "testBoundaryLength" testBoundaryLength
  , testCase "testDirectionTo" testDirectionTo
  ]

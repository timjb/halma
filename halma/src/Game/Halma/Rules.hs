{-# LANGUAGE OverloadedStrings #-}

module Game.Halma.Rules
  ( MoveRestriction (..)
  , RuleOptions (..)
  , possibleMoves
  , hasFinished
  ) where

import Game.Halma.Board

import Control.Monad (guard)
import Data.Aeson ((.=), (.:))
import Data.Default
import Data.Function (on)
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Monoid ((<>))
import Math.Geometry.Grid
import qualified Data.Aeson as A
import qualified Data.Set as S
import qualified Math.Geometry.Grid.HexagonalInternal as HexGrid

data MoveRestriction
  = Allowed     -- ^ moves of this kind of field are allowed
  | Temporarily -- ^ the player can pass the field but cannot occupy it
  | Forbidden   -- ^ the player can't pass or occupy the field
  deriving (Show, Eq)

instance A.ToJSON MoveRestriction where
  toJSON moveRestriction =
    case moveRestriction of
      Allowed -> "allowed"
      Temporarily -> "temporarily"
      Forbidden -> "forbidden"

instance A.FromJSON MoveRestriction where  
  parseJSON =
    A.withText "MoveRestriction" $ \text ->
      case text of
        "allowed"     -> pure Allowed
        "temporarily" -> pure Temporarily
        "forbidden"   -> pure Forbidden
        _ -> fail "expected 'allowed', 'temporarily' or 'forbidden'"

data RuleOptions
  = RuleOptions
  { movingBackwards :: MoveRestriction -- ^ May pieces be moved backwards?
  , invading :: MoveRestriction -- ^ May pieces be moved into other star corners?
  } deriving (Show, Eq)

instance A.ToJSON RuleOptions where
  toJSON rules =
    A.object
      [ "moving_backwards" .= A.toJSON (movingBackwards rules)
      , "invading" .= A.toJSON (invading rules)
      ]

instance A.FromJSON RuleOptions where
  parseJSON =
    A.withObject "RuleOptions" $ \o -> do
      bw <- o .: "moving_backwards"
      inv <- o .: "invading"
      pure $ RuleOptions { movingBackwards = bw, invading = inv }

instance Default RuleOptions where
  def = RuleOptions { movingBackwards = Temporarily, invading = Allowed }

filterForward :: (Int, Int) -> HalmaDirection -> [(Int, Int)] -> [(Int, Int)]
filterForward startPos halmaDir =
  filter $ ((>=) `on` rowsInDirection halmaDir) startPos

filterNonInvading :: Team -> HalmaGrid size -> [(Int, Int)] -> [(Int, Int)]
filterNonInvading team grid = filter $ \field -> all
  ((<= sideLength grid - 1) . abs . flip rowsInDirection field) 
  [leftOf team, leftOf (leftOf team)]
 
possibleStepMoves
  :: RuleOptions
  -> HalmaBoard size
  -> (Int, Int)
  -> [(Int, Int)]
possibleStepMoves ruleOptions halmaBoard startPos =
  case lookupHalmaBoard startPos halmaBoard of
    Nothing -> []
    Just piece ->
      ruleOptsFilters (pieceTeam piece) $
      filter ((== Nothing) . flip lookupHalmaBoard halmaBoard) $
      neighbours (getGrid halmaBoard) startPos
  where
    ruleOptsFilters team =
      setFilter movingBackwards (filterForward startPos team) .
      setFilter invading (filterNonInvading team (getGrid halmaBoard))
    setFilter ruleRestriction filterRule positions =
      case ruleRestriction ruleOptions of
        Allowed -> positions
        Temporarily -> filterRule positions
        Forbidden -> filterRule positions

possibleJumpMoves
  :: RuleOptions
  -> HalmaBoard size
  -> (Int, Int)
  -> [(Int, Int)]
possibleJumpMoves ruleOptions halmaBoard startPos =
  case lookupHalmaBoard startPos halmaBoard of
    Nothing -> []
    Just piece ->
      finalRuleOptsFilters (pieceTeam piece) $ S.toList $
      allJumpTargets (pieceTeam piece)
  where
    hexDirections =
      [ HexGrid.West, HexGrid.Northwest, HexGrid.Northeast
      , HexGrid.East, HexGrid.Southeast, HexGrid.Southwest
      ]
    isEmpty pos = isNothing (lookupHalmaBoard pos halmaBoard)
    isOccupied pos = isJust (lookupHalmaBoard pos halmaBoard)
    maybeJump p dir = do
      next1 <- neighbour (getGrid halmaBoard) p dir
      next2 <- neighbour (getGrid halmaBoard) next1 dir
      guard (isOccupied next1 && isEmpty next2)
      return next2
    nextJumpTargets team pos =
      continualRuleOptsFilters team pos $
      catMaybes (map (maybeJump pos) hexDirections)
    allJumpTargets team =
      iter S.empty (nextJumpTargets team startPos)
      where
        iter set [] = set
        iter set (pos:poss) =
          if S.member pos set
          then iter set poss
          else iter (S.insert pos set) (nextJumpTargets team pos ++ poss)
    finalRuleOptsFilters team =
      setFinalFilter movingBackwards (filterForward startPos team) .
      setFinalFilter invading (filterNonInvading team (getGrid halmaBoard))
    continualRuleOptsFilters team pos =
      setContinualFilter movingBackwards (filterForward pos team) .
      setContinualFilter invading (filterNonInvading team (getGrid halmaBoard))
    setFinalFilter ruleRestriction filterRule positions =
      case ruleRestriction ruleOptions of
        Allowed -> positions
        Temporarily -> filterRule positions
        Forbidden -> filterRule positions
    setContinualFilter ruleRestriction filterRule positions =
      case ruleRestriction ruleOptions of
        Allowed -> positions
        Temporarily -> positions
        Forbidden -> filterRule positions

-- | Computes all possible moves for a piece.
possibleMoves
  :: RuleOptions
  -> HalmaBoard size
  -> (Int, Int)
  -> [(Int, Int)]
possibleMoves = possibleStepMoves <> possibleJumpMoves

-- | Has a team all of it's pieces in the end zone?
hasFinished :: HalmaBoard size -> Team -> Bool
hasFinished halmaBoard team =
  all hasPieceFromTheRightTeam (endFields (getGrid halmaBoard) team)
  where
    hasTheRightTeam piece = pieceTeam piece == team
    hasPieceFromTheRightTeam pos =
      maybe False hasTheRightTeam (lookupHalmaBoard pos halmaBoard)

module Game.Halma.Rules
  ( MoveRestriction (..)
  , RuleOptions (..)
  , possibleMoves
  , hasFinished
  ) where

import Math.Geometry.Grid
import qualified Math.Geometry.Grid.HexagonalInternal as HI
import Game.Halma.Board
import Data.Default
import Data.Function (on)
import Data.Maybe (catMaybes)
import Control.Monad (guard)
import qualified Data.Set as S


data MoveRestriction =
    Allowed     -- ^ moves of this kind of field are allowed
  | Temporarily -- ^ the player can pass the field but cannot occupy it
  | Forbidden   -- ^ the player can't pass or occupy the field
  deriving (Show, Eq)

data RuleOptions =
  RuleOptions { movingBackwards :: MoveRestriction -- ^ May pieces be moved backwards?
              , invading :: MoveRestriction -- ^ May pieces be moved into other star corners?
              } deriving (Show, Eq)

instance Default RuleOptions where
  def = RuleOptions { movingBackwards = Temporarily
                    , invading = Allowed }

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
    Just team ->
      ruleOptsFilters $
      filter ((== Nothing) . flip lookupHalmaBoard halmaBoard) $
      neighbours (getGrid halmaBoard) startPos
      where ruleOptsFilters = setFilter (movingBackwards ruleOptions)
                                        (filterForward startPos team)
                            . setFilter (invading ruleOptions)
                                        (filterNonInvading team (getGrid halmaBoard))
            setFilter Allowed = const id
            setFilter _       = id

possibleJumpMoves
  :: RuleOptions
  -> HalmaBoard size
  -> (Int, Int)
  -> [(Int, Int)]
possibleJumpMoves ruleOptions halmaBoard startPos =
  case lookupHalmaBoard startPos halmaBoard of
    Nothing -> []
    Just team ->
      finalRuleOptsFilters $
      S.toList $ go S.empty (filteredJumpTargets startPos)
      where hexDirections =
              [ HI.West, HI.Northwest, HI.Northeast
              , HI.East, HI.Southeast, HI.Southwest
              ]
            isEmpty = (== Nothing) . flip lookupHalmaBoard halmaBoard
            maybeJump p dir = do
              next1 <- neighbour (getGrid halmaBoard) p dir
              next2 <- neighbour (getGrid halmaBoard) next1 dir
              guard $ not (isEmpty next1) && isEmpty next2
              return next2
            filteredJumpTargets p = continualRuleOptsFilters p $ jumpTargets p
            jumpTargets p = catMaybes $ map (maybeJump p) hexDirections
            go set [] = set
            go set (p:ps) =
              if S.member p set
              then go set ps
              else go (S.insert p set) (filteredJumpTargets p ++ ps)
            finalRuleOptsFilters =
                setFinalFilter (movingBackwards ruleOptions)
                               (filterForward startPos team)
              . setFinalFilter (invading ruleOptions)
                               (filterNonInvading team (getGrid halmaBoard))
            continualRuleOptsFilters pos =
                setContinualFilter (movingBackwards ruleOptions)
                                   (filterForward pos team)
              . setContinualFilter (invading ruleOptions)
                                   (filterNonInvading team (getGrid halmaBoard))
            setFinalFilter Temporarily = id
            setFinalFilter _           = const id
            setContinualFilter Forbidden = id
            setContinualFilter _         = const id

-- | Computes all possible moves for a piece.
possibleMoves
  :: RuleOptions
  -> HalmaBoard size
  -> (Int, Int)
  -> [(Int, Int)]
possibleMoves ruleOptions halmaBoard startPos =
  possibleStepMoves ruleOptions halmaBoard startPos ++
  possibleJumpMoves ruleOptions halmaBoard startPos

-- | Has a team all of it's pieces in the end zone?
hasFinished :: HalmaBoard size -> Team -> Bool
hasFinished halmaBoard team =
  all ((==) (Just team) . flip lookupHalmaBoard halmaBoard)
      (endFields (getGrid halmaBoard) team)

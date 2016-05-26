module Game.Halma.AI.Competitive
  ( aiMove
  ) where

import Game.Halma.Board
import Game.Halma.Rules
import Game.Halma.AI.Base

import Data.List (sortOn)

-- | The perspective of the first team as opposed to the second team.
type Perspective = (Team, Team)

flipPersp :: Perspective -> Perspective
flipPersp (t0, t1) = (t1, t0)

rate :: Perspective -> HalmaBoard size -> Rating
rate (t0, t1) board = rateTeam t0 board `against` rateTeam t1 board
  where
    (WinIn n) `against` _ = WinIn n
    _ `against` (WinIn n) = LossIn n
    (Rating r0) `against` (Rating r1) = Rating (r0 - r1)
    _ `against` _ = error "unexpected team rating indicating loss"

aiMove :: RuleOptions -> HalmaBoard size -> Perspective -> Move size
aiMove opts board persp =
  snd $ prunedMinMaxSearch 3 opts board persp Nothing 

-- | Find the best move or one that reaches the given bound.
prunedMinMaxSearch
  :: Int
  -> RuleOptions
  -> HalmaBoard size
  -> Perspective
  -> Maybe Rating
  -> (Rating, Move size)
prunedMinMaxSearch depth opts board persp mBound =
  go Nothing allOptions
  where
    allOptions =
      sortIfUseful $ do
        move <- allLegalMoves opts board (fst persp)
        pure (rate persp (outcome board move), move)
    sortIfUseful =
      if depth <= 2 then
        id
      else
        sortOn (flipRating . fst)
    go :: Maybe (Rating, Move size) -> [(Rating, Move size)] -> (Rating, Move size)
    go Nothing (option@(rating, _move):options) =
      if isWin rating then
        option
      else
        go (Just $ nextLevel option Nothing) options
    go (Just (currentMax, bestMove)) [] = (currentMax, bestMove)
    go (Just (currentMax, bestMove)) (option@(rating, move):options) =
      if isWin rating then
        option
      else if newRating <= currentMax then
        go (Just (currentMax, bestMove)) options
      else if boundReached newRating then
        (newRating, move)
      else
        go (Just (newRating, move)) options
      where newRating = fst $ nextLevel option (Just currentMax)
    go Nothing [] = error "no legal moves found"
    nextLevel (rating, move) mCurrentMax =
      let newRating =
            if depth <= 1 then
              rating
            else
              pushRating $ flipRating $ fst $
              prunedMinMaxSearch
                (depth-1) opts (outcome board move)
                (flipPersp persp) (fmap flipRating mCurrentMax)
      in (newRating, move)
    boundReached dep = maybe False (dep >=) mBound

isWin :: Rating -> Bool
isWin rating =
  case rating of
    WinIn _ -> True
    _ -> False

flipRating :: Rating -> Rating
flipRating rating =
  case rating of
    WinIn n -> LossIn n
    LossIn n -> WinIn n
    Rating r -> Rating (-r)

pushRating :: Rating -> Rating
pushRating rating =
  case rating of
    WinIn n -> WinIn (n+1)
    LossIn n -> WinIn (n+1)
    Rating r -> Rating r

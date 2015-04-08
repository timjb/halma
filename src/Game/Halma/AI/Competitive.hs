module Game.Halma.AI.Competitive
  ( aiMove
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Game.Halma.Board
import Game.Halma.Rules
import Game.Halma.AI.Base


-- The perspective of the first team as opposed to the second team.
type Perspective = (Team, Team)

flipPersp :: Perspective -> Perspective
flipPersp (t0, t1) = (t1, t0)


rating :: Perspective -> HalmaBoard size -> Rating
rating (t0, t1) board = teamRating t0 board `against` teamRating t1 board
  where (WinIn n) `against` _ = WinIn n
        _ `against` (WinIn n) = LossIn n
        (Rating r0) `against` (Rating r1) = Rating (r0 - r1)
        _ `against` _ = error "unexpected team rating indicating loss"


aiMove :: RuleOptions -> HalmaBoard size -> Perspective -> Move size
aiMove opts board persp = snd $
  prunedMinMaxSearch 3 opts board persp Nothing 


-- Find the best move or one that reaches the given bound.
prunedMinMaxSearch
  :: Int
  -> RuleOptions
  -> HalmaBoard size
  -> Perspective
  -> Maybe Rating
  -> (Rating, Move size)
prunedMinMaxSearch depth opts board persp mBound =
  go Nothing allOptions
  where allOptions = sortIfUseful [ (rating persp (outcome board move), move)
                                  | move <- allLegalMoves opts board (fst persp) ]
        sortIfUseful = if depth<=2 then id
                       else sortBy (flip $ comparing fst)
        go :: Maybe (Rating, Move size) -> [(Rating, Move size)] -> (Rating, Move size)
        go Nothing (option:options) =
          if isWin (fst option) then option
          else go (Just $ nextLevel option Nothing) options
        go (Just (currentMax, bestMove)) [] = (currentMax, bestMove)
        go (Just (currentMax, bestMove)) (option:options) =
          if isWin (fst option) then option
          else if newRating <= currentMax
               then go (Just (currentMax, bestMove)) options
               else if boundReached mBound newRating
                    then (newRating, snd option)
                    else go (Just (newRating, snd option)) options
          where newRating = fst $ nextLevel option (Just currentMax)
        go Nothing [] = error "no legal moves found"
        nextLevel option mCurrentMax =
            ( if depth>1
              then pushRating . flipRating . fst $
                prunedMinMaxSearch (depth-1) opts (outcome board (snd option))
                                   (flipPersp persp) (fmap flipRating mCurrentMax)
              else fst option
            , snd option )
        boundReached Nothing _ = False
        boundReached (Just bound) value = value >= bound


isWin :: Rating -> Bool
isWin (WinIn _) = True
isWin _ = False

flipRating :: Rating -> Rating
flipRating (WinIn n) = LossIn n
flipRating (LossIn n) = WinIn n
flipRating (Rating r) = Rating (-r)

pushRating :: Rating -> Rating
pushRating (WinIn n) = WinIn (n+1)
pushRating (LossIn n) = WinIn (n+1)
pushRating (Rating r) = Rating r

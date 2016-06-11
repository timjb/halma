{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Halma.TelegramBot.Move
  ( TargetModifier
  , showTargetModifier
  , PieceNumber
  , showPieceNumber
  , RowNumber (..)
  , internalToHumanRowNumber
  , humanToInternalRowNumber
  , radiusInRows
  , MoveCmd (..)
  , showMoveCmd
  , parseMoveCmd
  , moveToMoveCmd
  , CheckMoveCmdResult (..)
  , checkMoveCmd
  ) where

import Game.Halma.Board
import Game.Halma.Rules

import Data.Bifunctor (first)
import Data.Char (chr, ord, toUpper)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid ((<>))
import Data.Traversable (mapAccumL)
import Data.Tuple (swap)
import Data.Word (Word8)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Megaparsec as P

data TargetModifier
  = TargetModifier
  { _unTargetModifier :: Int
  } deriving (Show, Eq, Ord)

showTargetModifier :: TargetModifier -> T.Text
showTargetModifier (TargetModifier i) =
  case i of
    -1 -> "l"
    0 -> "c"
    1 -> "r"
    _ | i < -1 ->
      T.replicate (-i-1) "L"
    _ -> -- i > 1
      T.replicate (i-1) "R"

targetModifierParser :: P.Parsec P.Dec T.Text TargetModifier
targetModifierParser =
  P.choice
    [ TargetModifier . negate . (+1) <$> counting 'L'
    , TargetModifier (-1) <$ P.char 'l'
    , TargetModifier 0 <$ P.char' 'c'
    , TargetModifier 1 <$ P.char 'r'
    , TargetModifier . (+1) <$> counting 'R'
    ]
  where
    counting c = length <$> P.some (P.char c)

tagWithTargetModifier :: Traversable t => t a -> t (TargetModifier, a)
tagWithTargetModifier untagged =
  snd (mapAccumL accumFun firstTargetModifier untagged)
  where
    l = length untagged
    accumFun targetModifier val =
      (nextTargetModifier targetModifier, (targetModifier, val))
    firstTargetModifier = TargetModifier (- (l `div` 2))
    nextTargetModifier (TargetModifier i) =
      if i == -1 && even l then
        TargetModifier 1
      else
        TargetModifier (i+1)

type PieceNumber = Word8

pieceNumberToChar :: PieceNumber -> Char
pieceNumberToChar i = chr (ord 'A' + fromIntegral i - 1)

pieceNumberFromChar :: Char -> Maybe PieceNumber
pieceNumberFromChar c =
  let
    j = 1 + ord (toUpper c) - ord 'A'
  in if 1 <= j && j <= 15 then
    Just (fromIntegral j)
  else
    Nothing

showPieceNumber :: PieceNumber -> T.Text
showPieceNumber = T.singleton . pieceNumberToChar

pieceNumberParser :: P.Parsec P.Dec T.Text PieceNumber
pieceNumberParser = do
  c <- P.oneOf' (pieceNumberToChar <$> [1..15]) P.<?> "piece number"
  case pieceNumberFromChar c of
    Nothing ->
      fail "unexpected error while parsing piece number"
    Just i -> pure i

-- | Human readable (non-negative) row number
newtype RowNumber
  = RowNumber
  { unRowNumber :: Int
  } deriving (Show, Eq, Ord)

radiusInRows :: HalmaGrid -> Int
radiusInRows grid =
  case grid of
    SmallGrid -> 8
    LargeGrid -> 10

humanToInternalRowNumber :: HalmaGrid -> RowNumber -> Int
humanToInternalRowNumber grid (RowNumber i) = i - radiusInRows grid

internalToHumanRowNumber :: HalmaGrid -> Int -> RowNumber
internalToHumanRowNumber grid i = RowNumber (i + radiusInRows grid)

data MoveCmd
  = MoveCmd
  { movePieceNumber :: Word8 -- ^ number between 1 and 15
  , moveTargetRow :: RowNumber
  , moveTargetModifier :: Maybe TargetModifier
  } deriving (Show, Eq)

showMoveCmd :: MoveCmd -> T.Text
showMoveCmd moveCmd =
  showPieceNumber (movePieceNumber moveCmd) <>
  T.pack (show (unRowNumber (moveTargetRow moveCmd))) <>
  maybe "" showTargetModifier (moveTargetModifier moveCmd)

moveCmdParser :: P.Parsec P.Dec T.Text MoveCmd
moveCmdParser =
  MoveCmd
    <$> (pieceNumberParser <* P.space)
    <*> targetRowParser
    <*> P.optional (P.try (P.space *> targetModifierParser))
  where
    nonNegativeIntParser =
      RowNumber . read <$> P.some P.digitChar P.<?> "non negative integer"
    targetRowParser = nonNegativeIntParser

parseMoveCmd :: T.Text -> Either String MoveCmd
parseMoveCmd text =
  first P.parseErrorPretty $
    P.runParser moveCmdLineParser "halma move cmd" text
  where
    moveCmdLineParser = (P.space *> moveCmdParser) <* P.space <* P.eof

movesToRow
  :: RuleOptions
  -> HalmaBoard
  -> (Int, Int)
  -> Int
  -> [(TargetModifier, Move)]
movesToRow rules board startPos targetRow =
  let
    allEndPos = possibleMoves rules board startPos
    endPosInTargetRow = filter isInTargetRow allEndPos
    sortedEndPos = sortBy compareByXCoord endPosInTargetRow
    sortedMoves = mkMoveTo <$> sortedEndPos
  in
    tagWithTargetModifier sortedMoves
  where
    isInTargetRow (_x, y) = y == targetRow
    mkMoveTo endPos = Move { moveFrom = startPos, moveTo = endPos }
    compareByXCoord (x1, _) (x2, _) = compare x1 x2

moveToMoveCmd
  :: RuleOptions
  -> HalmaBoard
  -> Move
  -> Maybe MoveCmd
moveToMoveCmd rules board move = do
  let
    startPos = moveFrom move
  piece <- lookupHalmaBoard startPos board
  let
    targetRow = getRow (moveTo move)
    movesToTargetRow = movesToRow rules board startPos targetRow
  targetModifier <- lookup move (map swap movesToTargetRow)
  Just $
    MoveCmd
      { movePieceNumber = pieceNumber piece
      , moveTargetRow = internalToHumanRowNumber (getGrid board) targetRow
      , moveTargetModifier = Just targetModifier
      }
  where
    getRow (_x, y) = y


data CheckMoveCmdResult
  = MoveImpossible String
  | MoveFoundUnique Move
  | MoveSuggestions (NonEmpty (MoveCmd, Move))
  deriving (Show, Eq)

checkMoveCmd
  :: RuleOptions
  -> HalmaBoard
  -> Team
  -> MoveCmd
  -> CheckMoveCmdResult
checkMoveCmd rules board player moveCmd =
  case lookup pieceToMove allPieces of
    Nothing ->
      MoveImpossible $
        "Unexpected error: The piece " ++ show pieceToMove ++
        " is not on the Halma board!"
    Just startPos ->
      let
        mkMoveCmd tm = moveCmd { moveTargetModifier = Just tm }
        movesToTargetRow = movesToRow rules board startPos targetRow
      in
        case first mkMoveCmd <$> movesToTargetRow of
          [] ->
            MoveImpossible $
              "The selected piece can't be moved to row " ++
              show (unRowNumber (moveTargetRow moveCmd)) ++ "!"
          [(moveCmd', move)] ->
            case moveTargetModifier moveCmd of
              Nothing ->
                MoveFoundUnique move
              Just (TargetModifier 0) ->
                MoveFoundUnique move
              Just (TargetModifier _) ->
                MoveSuggestions $ pure (moveCmd', move)
          firstMove:restMoves@(_:_) ->
            let
              mMove = do
                targetModifier <- moveTargetModifier moveCmd
                lookup targetModifier movesToTargetRow
            in
              case mMove of
                Just move -> MoveFoundUnique move
                Nothing -> MoveSuggestions (firstMove :| restMoves)

  where
    allPieces = swap <$> M.toList (toMap board)
    pieceToMove =
      Piece
        { pieceTeam = player
        , pieceNumber = movePieceNumber moveCmd
        }
    targetRow = humanToInternalRowNumber (getGrid board) (moveTargetRow moveCmd)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Halma.TelegramBot.Move
  ( TargetModifier
  , showTargetModifier
  , MoveCmd (..)
  , showMoveCmd
  , parseMoveCmd
  , CheckMoveCmdResult (..)
  , checkMoveCmd
  ) where

import Game.Halma.Board
import Game.Halma.Rules

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..), sortBy, toList)
import Data.Monoid ((<>))
import Data.Traversable (mapAccumL)
import Data.Tuple (swap)
import Data.Word (Word8)
import Numeric (showHex)
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
    , TargetModifier 0 <$ P.char 'l'
    , TargetModifier 0 <$ P.char' 'c'
    , TargetModifier 0 <$ P.char 'r'
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

data MoveCmd
  = MoveCmd
  { movePieceNumber :: Word8 -- ^ number between 1 and 15
  , moveTargetRow :: Int
  , moveTargetModifier :: Maybe TargetModifier
  } deriving (Show, Eq)

showMoveCmd :: MoveCmd -> T.Text
showMoveCmd moveCmd =
  T.pack (showHex (movePieceNumber moveCmd) "") <> "-" <>
  T.pack (show (moveTargetRow moveCmd)) <>
  maybe "" showTargetModifier (moveTargetModifier moveCmd)

moveCmdParser :: P.Parsec P.Dec T.Text MoveCmd
moveCmdParser =
  MoveCmd
    <$> (pieceNumberParser <* P.space <* P.char '-' <* P.space)
    <*> targetRowParser
    <*> P.optional (P.try (P.space *> targetModifierParser))
  where
    pieceNumberParser = do
      c <- P.hexDigitChar
      when (c == '0') $
        fail "invalid piece number: 0"
      return (read ['0','x',c])
    nonNegativeIntParser =
      read <$> P.some P.digitChar P.<?> "non negative integer"
    targetRowParser = nonNegativeIntParser

parseMoveCmd :: T.Text -> Either String MoveCmd
parseMoveCmd text =
  first P.parseErrorPretty $
    P.runParser moveCmdLineParser "halma move cmd" text
  where
    moveCmdLineParser = (P.space *> moveCmdParser) <* P.space <* P.eof

data CheckMoveCmdResult
  = MoveImpossible String
  | MoveFoundUnique Move
  | MoveSuggestions (NonEmpty (TargetModifier, Move))

checkMoveCmd
  :: RuleOptions
  -> HalmaBoard size
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
        possibleEndPositions = possibleMoves rules board startPos
        mkMoveTo endPos = Move { moveFrom = startPos, moveTo = endPos }
      in
        case filter isInRightRow possibleEndPositions of
          [] ->
            MoveImpossible $
              "The selected piece can't be moved to row " ++
              show (moveTargetRow moveCmd) ++ "!"
          [endPos] ->
            case moveTargetModifier moveCmd of
              Nothing ->
                MoveFoundUnique (mkMoveTo endPos)
              Just (TargetModifier 0) ->
                MoveFoundUnique (mkMoveTo endPos)
              Just (TargetModifier _) ->
                MoveSuggestions $ pure (TargetModifier 0, mkMoveTo endPos)
          firstEndPos:restEndPos@(_:_) ->
            let
              sortedEndPos = sortBy compareByXCoord (firstEndPos :| restEndPos)
              sortedMoves = mkMoveTo <$> sortedEndPos
              taggedMoves = tagWithTargetModifier sortedMoves
              mMove = do
                targetModifier <- moveTargetModifier moveCmd
                lookup targetModifier (toList taggedMoves)
            in
              case mMove of
                Just move -> MoveFoundUnique move
                Nothing -> MoveSuggestions taggedMoves
  where
    allPieces = swap <$> M.toList (toMap board)
    pieceToMove =
      Piece
        { pieceTeam = player
        , pieceNumber = movePieceNumber moveCmd
        }
    isInRightRow (_, y) = y == moveTargetRow moveCmd
    compareByXCoord (x1, _) (x2, _) = compare x1 x2
    

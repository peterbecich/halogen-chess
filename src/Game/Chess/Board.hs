{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}

module Game.Chess.Board where

import Prelude

import Data.Aeson
import GHC.Generics

import Data.Bits (Bits (unsafeShiftL, (.|.)))

import Game.Chess
    (IsSquare (toIndex), Ply (..), Position, Sq (..), doPly, fromFEN, toFEN)
import Game.Chess.Move (Move (Move))
import Game.Chess.Orphans ()

data Board = Board [Sq]

allPieces :: Board
allPieces = Board $ enumFrom minBound

deriving instance Generic Board

deriving instance ToJSON Board
deriving instance FromJSON Board

-- copied from https://github.com/peterbecich/chessIO/blob/0d61d8352096e4f893c13a8ff7b275b9a76d2de9/src/Game/Chess/Internal.hs#L237-L239
move :: (IsSquare from, IsSquare to) => from -> to -> Ply
move (toIndex -> from) (toIndex -> to) =
  Ply $ fromIntegral to .|. fromIntegral from `unsafeShiftL` 6

checkMove :: Sq -> Sq -> Position -> Position
checkMove from to start = doPly start $ move from to

checkMove' :: Move -> String
checkMove' (Move fenPosition from to) = case fromFEN fenPosition of
  Nothing       -> fenPosition
  Just position -> toFEN $ checkMove from to position

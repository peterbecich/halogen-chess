{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric         #-}

module Game.Chess.Board where

import Prelude

import           Data.Aeson
import           GHC.Generics

import Game.Chess.Orphans ()
import Game.Chess (Sq (..))

data Board = Board [Sq]

allPieces :: Board
allPieces = Board $ enumFrom minBound

deriving instance Generic Board

deriving instance ToJSON Board
deriving instance FromJSON Board

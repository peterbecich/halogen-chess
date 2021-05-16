{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Chess.Board where

import Prelude

import Data.Aeson
import GHC.Generics

import Game.Chess (Sq (..))
import Game.Chess.Orphans ()

data Board = Board [Sq]

allPieces :: Board
allPieces = Board $ enumFrom minBound

deriving instance Generic Board

deriving instance ToJSON Board
deriving instance FromJSON Board

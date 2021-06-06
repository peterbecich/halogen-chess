{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Chess.Move where

import Prelude

import Game.Chess (Position, Sq)
import Game.Chess.Orphans ()

import Data.Aeson
import GHC.Generics

data Move = Move { fenPosition :: String, from :: Sq, to :: Sq }

deriving instance Generic Move

deriving instance ToJSON Move
deriving instance FromJSON Move

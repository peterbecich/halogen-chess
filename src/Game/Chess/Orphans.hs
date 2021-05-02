{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric         #-}

module Game.Chess.Orphans where

import           Data.Aeson
import           GHC.Generics

import Game.Chess (Color (..), PieceType (..), Sq (..))


deriving instance Generic Sq

deriving instance ToJSON Sq
deriving instance FromJSON Sq

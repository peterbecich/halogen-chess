{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric         #-}

module Game.Chess.Orphans where

import           Data.Aeson
import           GHC.Generics

import Game.Chess (Color (..), PieceType (..), Sq (..))


deriving instance Generic Sq
deriving instance Generic Color
deriving instance Generic PieceType

deriving instance ToJSON Sq
deriving instance FromJSON Sq

deriving instance ToJSON Color
deriving instance FromJSON Color

deriving instance ToJSON PieceType
deriving instance FromJSON PieceType

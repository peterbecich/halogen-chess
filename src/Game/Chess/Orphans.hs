{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Game.Chess.Orphans where

import Data.Aeson
import GHC.Generics

import Game.Chess
    (Color (..), PieceType (..), Ply (..), Position, Square (..), fromFEN, toFEN)

deriving instance ToJSON Square
deriving instance FromJSON Square

deriving instance ToJSON Color
deriving instance FromJSON Color

deriving instance ToJSON PieceType
deriving instance FromJSON PieceType

deriving instance ToJSON Ply
deriving instance FromJSON Ply

instance ToJSON Position where
  toJSON = toJSON . toFEN

instance FromJSON Position where
  parseJSON pos = do
    x :: String <- parseJSON pos
    case fromFEN x of
      Nothing -> fail "failed to parse FEN"
      Just x' -> pure x'

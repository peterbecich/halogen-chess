{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ViewPatterns        #-}

module Game.Chess.Board where

import Prelude

import qualified Control.Exception as Exc
import           Control.Monad (forM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import           Data.Bits (Bits (unsafeShiftL, (.|.)))
import           GHC.Generics

import Game.Chess
    ( Ply (..)
    , Position
    , Square
    , doPly
    , fromFEN
    , legalPlies
    , toFEN
    , unsafeDoPly
    )
import Game.Chess.Move (Move (Move))
import Game.Chess.Orphans ()

newtype Board = Board [Square]

allPieces :: Board
allPieces = Board $ enumFromTo minBound maxBound

deriving instance Generic Board

deriving instance ToJSON Board
deriving instance FromJSON Board

-- copied from https://github.com/peterbecich/chessIO/blob/30455cc9e200240125986827dfeb363542e0d4a2/src/Game/Chess/Internal.hs#L249-L251
move :: Square -> Square -> Ply
move _ _ = undefined
  -- Ply $ fromIntegral to .|. fromIntegral from `unsafeShiftL` 6

-- copied
-- https://github.com/peterbecich/chessIO/blob/0d61d8352096e4f893c13a8ff7b275b9a76d2de9/src/Game/Chess/Internal.hs#L373-L376
-- can't use `doPly`; can't capture `error`, intended to be irrecoverable
doPly' :: Position -> Ply -> Maybe Position
doPly' p m
  | m `elem` legalPlies p = Just $ unsafeDoPly p m
  | otherwise             = Nothing

checkMove
  :: Square -> Square -> Position -> Maybe Position
checkMove from to start = doPly' start mv
  where
    mv = move from to

checkMove' :: Move -> Maybe String
checkMove' (Move fenPosition from to) = do
  let
    mPosition :: Maybe Position = fromFEN fenPosition
  case mPosition of
    Nothing -> Nothing
    Just position -> do
      toFEN <$> checkMove from to position

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}

module Bridge where

import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Language.PureScript.Bridge
    (BridgePart, Language (Haskell), SumType, defaultBridge, mkSumType)
import Language.PureScript.Bridge.PSTypes ()

import Game.Chess
    ( Color (..)
    , File (..)
    , PieceType (..)
    , Ply (..)
    , Position (..)
    , Rank (..)
    , Square (..)
    )

import Game.Chess.Board (Board)
import Game.Chess.Move (Move)

myBridge :: BridgePart
myBridge = defaultBridge

myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType (Proxy :: Proxy Color)
  , mkSumType (Proxy :: Proxy Square)
  , mkSumType (Proxy :: Proxy PieceType)
  , mkSumType (Proxy :: Proxy Board)
  , mkSumType (Proxy :: Proxy Move)
  , mkSumType (Proxy :: Proxy Rank)
  , mkSumType (Proxy :: Proxy File)
  -- , mkSumType (Proxy :: Proxy Ply)
  -- , mkSumType (Proxy :: Proxy Position)
  ]

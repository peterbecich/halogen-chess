module Game.Store where

import Prelude

import Game.Chess.Board (Board(Board))

type Store =
  { board :: Board
  , fenPosition :: String
  }

initialStore :: Store
initialStore =
  { board: Board mempty
  , fenPosition: mempty
  }

data Action = SetBoard Board | SetFenPosition String

reduce :: Store -> Action -> Store
reduce store = case _ of
  SetBoard b       -> store { board = b }
  SetFenPosition s -> store { fenPosition = s }

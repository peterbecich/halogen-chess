module Game.Store where

import Prelude

import Game.Chess.Board (Board(Board))

type Store = { board :: Board }

initialStore :: Store
initialStore = { board: Board mempty }

data Action = SetBoard Board

reduce :: Store -> Action -> Store
reduce _ = case _ of
  SetBoard b -> { board: b }

module Game.Store where

import Prelude

import Game.Chess.Board (Board(Board))
import Game.Sq(Sq'(Sq'), State)
import Game.Chess.Internal.Square (Sq)

import Data.HashMap (HashMap, empty, insert, delete)

type Store =
  { board :: Board
  , fenPosition :: String
  , squares :: HashMap Sq' State
  }

initialStore :: Store
initialStore =
  { board: Board mempty
  , fenPosition: mempty
  , squares: empty
  }

data Action
  = SetBoard Board
  | SetFenPosition String
  | SetSquare Sq State
  | ClearSquare Sq

reduce :: Store -> Action -> Store
reduce store = case _ of
  SetBoard b       -> store { board = b }
  SetFenPosition s -> store { fenPosition = s }
  SetSquare sq squareState ->
    store { squares = insert (Sq' sq) squareState store.squares }
  ClearSquare sq   -> store { squares = delete (Sq' sq) store.squares }

module Game.HTML.Piece where

import Prelude

import Data.Maybe (Maybe(Just,Nothing))
import Data.Tuple (Tuple(Tuple))
import Game.Chess.Internal (PieceType(..), Color(Black,White))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

pieceHTML ::
     forall t10 t11. Maybe (Tuple Color PieceType)
  -> HH.HTML t10 t11
pieceHTML Nothing      = HH.fromPlainHTML $ HH.div_ mempty
pieceHTML (Just (Tuple color piece)) = HH.fromPlainHTML
  $ HH.img [ HP.src $ root <> p piece <> c color <> suffix ]
  where
    root = "images/Chess_"

    p Pawn   = "p"
    p Knight = "k"
    p Bishop = "b"
    p Rook   = "r"
    p Queen  = "q"
    p King   = "k"

    c Black = "d"
    c White = "l"

    suffix = "t45.svg"

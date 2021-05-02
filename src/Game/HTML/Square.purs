module Game.HTML.Square where

import Prelude

import Game.Chess.Internal (Color(..))
import Game.Chess.Internal.Square (Sq)

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Halogen.HTML.CSS as CSS
import CSS.Color as CSS.Color
import CSS.Background as Background
import Halogen.HTML as HH

square :: Color -> Sq -> HH.PlainHTML
square color sq = HH.div [ color' ] [ HH.text (stringify (encodeJson sq)) ] where
  color' = CSS.style $ case color of
    Black -> Background.backgroundColor $ CSS.Color.lighten 0.6 CSS.Color.black
    White -> Background.backgroundColor $ CSS.Color.white

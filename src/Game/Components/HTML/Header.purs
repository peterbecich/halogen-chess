module Game.Components.HTML.Header where

import Halogen.HTML as HH

header :: forall i p. HH.HTML i p
header =
  HH.header_
    [ HH.div_
      [ HH.h2_
        [ HH.text "Chess" ]
      ]
    ]

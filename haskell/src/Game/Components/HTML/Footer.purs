module Game.Components.HTML.Footer where

import Halogen.HTML as HH

footer :: forall i p. HH.HTML i p
footer =
  HH.footer_
    [ HH.div_
      [ HH.span_
        [ HH.text "Footer" ]
      ]
    ]

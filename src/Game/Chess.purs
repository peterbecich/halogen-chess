module Game.Chess where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

data Action
  = CheckButtonState

type State =
  { toggleCount :: Int
  , buttonState :: Maybe Boolean
  }

type ChildSlots = ( )

_button :: Proxy "button"
_button = Proxy

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  { toggleCount: 0
  , buttonState: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.p_
        [ HH.text ("Button has been toggled " <> show state.toggleCount <> " time(s)") ]
    , HH.p_
        [ HH.text
            $ "Last time I checked, the button was: "
            <> (maybe "(not checked yet)" (if _ then "on" else "off") state.buttonState)
            <> ". "
        , HH.button
            [ ]
            [ HH.text "Check now" ]
        ]
    ]

handleAction ::forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  CheckButtonState -> pure unit
    -- buttonState <- H.request _button unit Button.IsOn
    -- H.modify_ (_ { buttonState = buttonState })

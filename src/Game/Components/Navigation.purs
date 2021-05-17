module Game.Components.Navigation where

import Prelude

import Data.Maybe (Maybe(..))
import Prim (Boolean, Int, Row, Type, Array)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Effect.Aff.Class (class MonadAff)
import Game.Routes (Route(..))

data Action
  = GoToChess
  | GoToPageB
  | GoToPageC

data Output = GoTo Route

data Query a = Foo

type NavigationSlot = H.Slot Query Output

type State = { }

initialState :: forall i. i -> State
initialState _ = { }

type ChildSlots :: forall k. Row k
type ChildSlots = ()

_navigation :: Proxy "navigation"
_navigation = Proxy

component ::
     forall i m. MonadAff m
  => H.Component Query i Output m
component =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval
      $ H.defaultEval
        { handleAction = handleAction
        , handleQuery  = \_ -> pure Nothing
        , initialize = Nothing
        }
    }

render ::
     forall m. State
  -> H.ComponentHTML Action ChildSlots m
render _ =
  HH.nav_
    [ HH.button [ HE.onClick \_ -> GoToChess ] [ HH.text "Play Chess" ]
    , HH.button [ HE.onClick \_ -> GoToPageB ] [ HH.text "Page B" ]
    , HH.button [ HE.onClick \_ -> GoToPageC ] [ HH.text "Page C" ]
    ]

handleAction ::
     forall m. MonadAff m
  => Action
  -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  GoToChess -> H.raise $ GoTo PlayChess
  GoToPageB -> H.raise $ GoTo PageB
  GoToPageC -> H.raise $ GoTo PageC

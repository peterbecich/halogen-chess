module Game.Components.Navigation where

import Prelude

import Data.Maybe (Maybe(..))
import Prim (Boolean, Int, Row, Type, Array)

import Halogen.HTML.CSS as HCSS
import CSS as CSS
import CSS.Border as CSS.Border
import CSS.Size as CSS.Size
import Color as Colors
import CSS.Color as CSS.Color
import CSS.Display as CSS.Display
import CSS.Background as CSS.Background
import CSS.Geometry as CSS.Geometry
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

data Query :: forall k. k -> Type
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
render _ = HH.nav [ buttons ]
  [ HH.div
    [ border, HE.onClick \_ -> GoToChess ] [ HH.text "Play Chess" ]
  , HH.div
    [ border, HE.onClick \_ -> GoToPageB ] [ HH.text "Page B" ]
  , HH.div
    [ border, HE.onClick \_ -> GoToPageC ] [ HH.text "Page C" ]
  ]
  where
    blue = Colors.rgb 0 0 255
    -- https://www.w3schools.com/css/css_boxmodel.asp
    -- https://pursuit.purescript.org/packages/purescript-css/5.0.1/docs/CSS
    buttons = HCSS.style do
      -- CSS.Display.display CSS.Display.flex
      CSS.Geometry.paddingLeft $ CSS.Size.pt 10.0
      CSS.Geometry.paddingRight $ CSS.Size.pt 10.0
      CSS.Geometry.paddingTop $ CSS.Size.pt 10.0
      CSS.Geometry.paddingBottom $ CSS.Size.pt 10.0
      CSS.Geometry.marginLeft $ CSS.Size.pt 10.0
      CSS.Geometry.marginRight $ CSS.Size.pt 10.0
      CSS.Geometry.marginTop $ CSS.Size.pt 10.0
      CSS.Geometry.marginBottom $ CSS.Size.pt 10.0
      -- CSS.Geometry.marginLeft $ CSS.Size.pt 60.0
      CSS.Geometry.width $ CSS.Size.pt 350.0
      CSS.Geometry.height $ CSS.Size.pt 60.0
      CSS.Background.backgroundColor $ CSS.Color.lighten 0.4 blue


    border = HCSS.style do
      CSS.float CSS.floatLeft
      CSS.Geometry.marginLeft $ CSS.Size.pt 10.0
      CSS.Geometry.width $ CSS.Size.pt 90.0
      CSS.Geometry.height $ CSS.Size.pt 30.0
      CSS.Background.backgroundColor $ CSS.Color.lighten 0.3 blue
      CSS.Border.border
        CSS.Border.solid
        (CSS.Size.px 2.0)
        CSS.Color.black


handleAction ::
     forall m. MonadAff m
  => Action
  -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  GoToChess -> H.raise $ GoTo PlayChess
  GoToPageB -> H.raise $ GoTo PageB
  GoToPageC -> H.raise $ GoTo PageC

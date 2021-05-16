module Game.Components.Router where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Effect.Console (log)
import Effect.Aff.Class (class MonadAff)
import Halogen.HTML as HH

import Game.Routes (Route(..))
import Game.Components.Utils (OpaqueSlot)
import Game.Components.Chessboard as Chessboard
import Game.Components.Navigation as Navigation
import Game.Components.HTML.Header as Header
import Game.Components.HTML.Footer as Footer

type State = Maybe Route

data Query a
  = Navigate Route a

data Action
  = Initialize
  | HandleNavOutput Navigation.Output


type Slots =
  ( navigation :: Navigation.NavigationSlot Unit
  , chessboard :: OpaqueSlot Unit
  , pageB      :: OpaqueSlot Unit
  , pageC      :: OpaqueSlot Unit
  )

component
  :: forall query input output m
   . MonadAff m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval
      $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }
  where
  initialState :: input -> State
  initialState _ = Just PlayChess

  render :: State -> H.ComponentHTML Action Slots m
  render mRoute = HH.div_
    [ HH.div_
      [ Header.header
      , HH.slot Navigation._navigation unit Navigation.component {} HandleNavOutput
      , body
      , Footer.footer
      ]
    ]
    where
      body = case mRoute of
        Nothing -> HH.div_ mempty
        Just PlayChess -> HH.slot Chessboard._chessboard unit Chessboard.component {} absurd
        Just PageB     -> HH.div_ [ HH.text "Page B" ]
        Just PageC     -> HH.div_ [ HH.text "Page C" ]

  handleAction
    :: Action
    -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    HandleNavOutput (Navigation.GoTo route) -> do
      H.liftEffect $ log $ "GoTo"
      H.put $ Just route
    _ -> pure unit

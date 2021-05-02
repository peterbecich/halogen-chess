module Game.Chess where

import Prelude

import Data.Lens (view)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Effect.Class.Console (logShow)
import Effect.Aff.Class (class MonadAff, liftAff)
import Affjax (Error, Response, get)
import Affjax.ResponseFormat (json)
import Data.Either (Either(Left, Right))
import Data.Traversable (for, for_)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Options (defaultOptions)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Game.Chess.Internal.Square (Sq)
import Data.Argonaut.Core (Json)
import Game.Chess.Board (Board(Board), _Board)
import Game.Chess.Internal (Color(..))
import Game.HTML.Square (square)
import Halogen.HTML.CSS as CSS
import CSS.Display as CSS.Display
import Halogen.HTML (ClassName(..))
import Halogen.HTML.Properties as HP

data Action
  = CheckButtonState
  | Initialize

type State =
  { toggleCount :: Int
  , buttonState :: Maybe Boolean
  , board       :: Board
  }

type ChildSlots = ( )

_button :: Proxy "button"
_button = Proxy

component :: forall q i o m. MonadAff m => H.Component q i o m
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

initialState :: forall i. i -> State
initialState _ =
  { toggleCount: 0
  , buttonState: Nothing
  , board: Board mempty
  }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state = HH.fromPlainHTML $ flip HH.div h $ s
  where
    b :: Array Sq
    b = view _Board state.board
    h :: Array HH.PlainHTML
    h = map (square Black) b

    g = pure $ CSS.style do
      -- grid properties not supported
      -- https://github.com/purescript-contrib/purescript-css/issues/112
      CSS.Display.display CSS.Display.grid
    -- https://github.com/JordanMartinez/learn-halogen/blob/v5.0.2/src/01-Static-HTML/06-Adding-CSS.purs
    s = g <> [ HP.classes [ ClassName "board" ] ]

handleAction ::
     forall o m. MonadAff m
  => Action
  -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Initialize -> do
    logShow "Initialize"
    boardResponse :: Either Error (Response Json) <-
      liftAff $ get json "/board"
    case boardResponse of
      Left _ -> logShow "no board response"
      _ -> pure unit
    for_ boardResponse $ \board' -> do
      for_ (genericDecodeAeson defaultOptions board'.body)
        $ \(b :: Board) -> do
          logShow "got Board"
          H.modify _ {board = b}

  CheckButtonState -> pure unit
    -- buttonState <- H.request _button unit Button.IsOn
    -- H.modify_ (_ { buttonState = buttonState })

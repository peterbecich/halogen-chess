module Game.Components.Chessboard where

import Prelude

import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options as Argonaut
import Halogen as H
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Effect.Class.Console (logShow)
import Effect.Aff.Class (class MonadAff, liftAff)
import Affjax (Error, Response, get)
import Affjax.ResponseFormat (json)
import Data.Either (Either(Left))
import Data.Traversable (for_)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Options (defaultOptions)
import Game.Chess.Internal.Square (Sq)
import Game.Chess.Board (Board(Board), _Board)
import Game.Components.Square as Square
import CSS as CSS
import Halogen.HTML.CSS as HCSS
import CSS.Display as CSS.Display


data Action
  = CheckButtonState
  | Initialize
  | ReceiveSquare Square.Output

type State =
  { toggleCount     :: Int
  , buttonState     :: Maybe Boolean
  , board           :: Board
  , sourceSelection :: Maybe Sq
  , destinationSelection :: Maybe Sq
  }

data Sq' = Sq' Sq
instance encodeJsonSq' :: EncodeJson Sq' where
  encodeJson = genericEncodeAeson Argonaut.defaultOptions
derive instance gensq :: Generic Sq' _
instance showsq :: Show Sq' where
  show x = stringify $ encodeJson x
instance eqsq :: Eq Sq' where
  eq x y = eq (show x) (show y)
instance ordsq :: Ord Sq' where
  compare x y = compare (show x) (show y)

type ChildSlots = ( square :: Square.SquareSlot Sq' )

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
  , sourceSelection: Nothing
  , destinationSelection: Nothing
  }

render ::
     forall m. MonadAff m
  => State
  -> H.ComponentHTML Action ChildSlots m
render state = HH.div style h
  where
    b :: Array Sq
    b = view _Board state.board
    h :: Array (HH.ComponentHTML Action ChildSlots m)
    h = flip map b $ \sq ->
      HH.slot Square._square (Sq' sq) (Square.component sq) sq ReceiveSquare

    style = pure $ HCSS.style do
      -- grid properties not supported; have to set manually
      -- https://github.com/purescript-contrib/purescript-css/issues/112
      CSS.key (CSS.fromString "grid-template-columns") "50px 50px 50px 50px 50px 50px 50px 50px"
      CSS.key (CSS.fromString "grid-template-rows") "50px 50px 50px 50px 50px 50px 50px 50px "
      CSS.Display.display CSS.Display.grid

handleAction ::
     forall o m. MonadAff m
  => Action
  -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  ReceiveSquare (Square.Clicked sq) -> do
    {sourceSelection, destinationSelection} <- H.get
    -- logShow
    case Tuple sourceSelection destinationSelection of
      Tuple (Just src) (Just dst) -> do
        let
          srcSlot = Sq' src
          dstSlot = Sq' dst
        H.tell Square._square srcSlot (pure Square.Unselect)
        H.tell Square._square dstSlot (pure Square.Unselect)
        void $ H.modify _ { sourceSelection = Just sq, destinationSelection = Nothing }
        H.tell Square._square (Sq' sq) (pure Square.Select)
        pure unit
      Tuple (Just _) Nothing -> do
        H.tell Square._square (Sq' sq) (pure Square.Select)
        void $ H.modify _ { destinationSelection = Just sq }
        pure unit
      Tuple Nothing (Just _) -> do
        void $ H.modify _ { sourceSelection = Nothing, destinationSelection = Nothing }
        pure unit
      Tuple Nothing Nothing -> do
        H.tell Square._square (Sq' sq) (pure Square.Select)
        void $ H.modify _ { sourceSelection = Just sq }
        pure unit

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

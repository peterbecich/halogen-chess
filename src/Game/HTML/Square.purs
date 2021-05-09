module Game.HTML.Square where

import Prelude

import Game.Chess.Internal (Color(..), PieceType)
import Game.Chess.Internal.Square (Sq)
import Data.Maybe (Maybe(..))
import Data.Argonaut.Encode.Class (encodeJson)
import Prim (Boolean, Int, Row, Type, Array)
import Data.Tuple (Tuple(Tuple))
import Effect.Console (log)
import Data.Argonaut.Core (Json)
import Affjax.RequestBody as RequestBody
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)

import Data.Either (Either)
import Data.Argonaut.Aeson.Options (defaultOptions)
import Data.Traversable (for, for_)
import Halogen.HTML.CSS as CSS
import CSS.Color as CSS.Color
import Affjax (Error, Response, post)
import Affjax.ResponseFormat (json)
import CSS.Background as Background
import CSS.Border as CSS.Border
import CSS.Size as CSS.Size
import Halogen as H
import Halogen.HTML.Properties as HP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Game.HTML.Piece as Piece

data Action
  = Initialize
  | Click

data Query :: forall k. k -> Type
data Query a = Foo

data Output = Clicked

type SquareSlot = H.Slot Query Output

type State =
  { sq :: Sq
  , coordinates :: Tuple Int Int
  , color :: Color
  , piece :: Maybe (Tuple Color PieceType)
  , selected :: Boolean
  }

initialState :: forall i. Sq -> i -> State
initialState sq' _ =
  { sq: sq'
  , coordinates: Tuple 1 2
  , color: White
  , piece: Nothing
  , selected: false
  }

type ChildSlots :: forall k. Row k
type ChildSlots = ()

_square :: Proxy "square"
_square = Proxy

component ::
     forall q i o m. MonadAff m
  => Sq
  -> H.Component q i o m
component sq' =
  H.mkComponent
    { initialState: initialState sq'
    , render: square
    , eval: H.mkEval
      $ H.defaultEval
        { handleAction = handleAction sq'
        , initialize = Just Initialize
        }
    }

coordinatesToCSS :: Tuple Int Int -> Array HH.ClassName
coordinatesToCSS (Tuple x y) =
  [ HH.ClassName "board"
  , HH.ClassName ("row-" <> show (8 - x))
  , HH.ClassName ("column-" <> show (y + 1))
  ]

square ::
     forall m. State
  -> H.ComponentHTML Action ChildSlots m
square { sq, coordinates, color, piece, selected } =
  HH.div
    [ border
    , HP.classes (coordinatesToCSS coordinates)
    , HE.onClick \_ -> Click
    ]
    [ Piece.pieceHTML piece
    ]
  where
  color' = case color of
    Black ->
      Background.backgroundColor $ CSS.Color.lighten 0.6 CSS.Color.black
    White ->
      Background.backgroundColor $ CSS.Color.white

  border = case selected of
    false -> CSS.style color'
    true  -> do
      CSS.style do
        color'
        CSS.Border.border
          CSS.Border.dashed
          (CSS.Size.px 3.0)
          CSS.Color.blue

handleAction ::
     forall o m. MonadAff m
  => Sq
  -> Action
  -> H.HalogenM State Action ChildSlots o m Unit
handleAction sq = case _ of
  Click -> do
    { piece } <- H.get
    case piece of
      Nothing -> pure unit
      Just _  -> do
        { selected } <- H.get
        void $ H.modify _ { selected = not selected }
        H.liftEffect $ log $ "click " <> show (not selected)
        pure unit
  Initialize -> do
    coordinateResponse :: Either Error (Response Json) <-
      liftAff $ post json "/rf" (Just (RequestBody.json (encodeJson sq)))
    for_ coordinateResponse $ \res -> do
      for (genericDecodeAeson defaultOptions res.body)
        $ \(coordinates' :: Tuple Int Int) -> do
          H.modify _ { coordinates = coordinates' }

    colorResponse :: Either Error (Response Json) <-
      liftAff $ post json "/color" (Just (RequestBody.json (encodeJson sq)))
    for_ colorResponse $ \res -> do
      for (genericDecodeAeson defaultOptions res.body)
        $ \(color' :: Color) -> do
          H.modify _ { color = color' }

    startingPosition :: Either Error (Response Json) <-
      liftAff $ post json "/pieceAtStartingPosition" (Just (RequestBody.json (encodeJson sq)))
    for_ startingPosition $ \res -> do
      for (genericDecodeAeson defaultOptions res.body)
        $ \(tuple :: Tuple Color PieceType) -> do
          H.modify _ { piece = Just tuple }

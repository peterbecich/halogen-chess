module Game.Components.Square where

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
import Data.Traversable (for_)
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
import Game.Components.HTML.Piece as Piece

data Action
  = Initialize
  | Click

data Query :: forall k. k -> Type
data Query a = Unselect | Select

data Output = Clicked Sq

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
     forall i m. MonadAff m
  => Sq
  -> H.Component Query i Output m
component sq' =
  H.mkComponent
    { initialState: initialState sq'
    , render: square
    , eval: H.mkEval
      $ H.defaultEval
        { handleAction = handleAction sq'
        , handleQuery  = handleQuery
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
square { coordinates, color, piece, selected } =
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

handleQuery ::
     forall m a. Query a
  -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  Unselect -> do
    H.modify_ _ { selected = false }
    pure Nothing
  Select -> do
    H.modify_ _ { selected = true }
    pure Nothing

handleAction ::
     forall m. MonadAff m
  => Sq
  -> Action
  -> H.HalogenM State Action ChildSlots Output m Unit
handleAction sq = case _ of
  Click -> do
    -- { piece } <- H.get
    -- for_ piece $ const do
    { selected } <- H.get
    H.liftEffect $ log $ "click " <> show (not selected)
    H.raise $ Clicked sq
  Initialize -> do
    coordinateResponse :: Either Error (Response Json) <-
      liftAff $ post json "/rf" $ Just $ RequestBody.json $ encodeJson sq
    for_ coordinateResponse $ \res -> do
      for_ (genericDecodeAeson defaultOptions res.body)
        $ \(coordinates :: Tuple Int Int) -> do
          H.modify_ _ { coordinates = coordinates }

    colorResponse :: Either Error (Response Json) <-
      liftAff $ post json "/color" $ Just $ RequestBody.json $ encodeJson sq
    for_ colorResponse $ \res -> do
      for_ (genericDecodeAeson defaultOptions res.body)
        $ \(color :: Color) -> do
          H.modify_ _ { color = color }

    startingPosition :: Either Error (Response Json) <-
      liftAff $ post json "/pieceAtStartingPosition" $ Just $ RequestBody.json $ encodeJson sq
    for_ startingPosition $ \res -> do
      for_ (genericDecodeAeson defaultOptions res.body)
        $ \(tuple :: Tuple Color PieceType) -> do
          H.modify_ _ { piece = Just tuple }

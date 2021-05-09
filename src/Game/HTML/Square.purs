module Game.HTML.Square where

import Prelude

import Game.Chess.Internal (Color(..), PieceType(Pawn))
import Game.Chess.Internal.Square (Sq)
import Data.Maybe (Maybe(..), maybe)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Tuple (Tuple(Tuple))
import Data.Argonaut.Core (Json)
import Affjax.RequestBody as RequestBody
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Effect.Class.Console (logShow)
import Data.Either (Either(Left, Right))
import Data.Argonaut.Aeson.Options (defaultOptions)
import Data.Traversable (for, for_)
import Halogen.HTML.CSS as CSS
import CSS.Color as CSS.Color
import Affjax (Error, Response, post)
import Affjax.ResponseFormat (json)
import CSS.Background as Background
import Halogen as H
import Halogen.HTML.Properties as HP
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Game.HTML.Piece as Piece

data Action = Initialize

data Query :: forall k. k -> Type
data Query a = Foo

data Output = Clicked

type SquareSlot = H.Slot Query Output

type State =
  { sq :: Sq
  , coordinates :: Tuple Int Int
  , color :: Color
  , piece :: Maybe (Tuple Color PieceType)
  }

initialState :: forall i. Sq -> i -> State
initialState sq' _ =
  { sq: sq'
  , coordinates: Tuple 1 2
  , color: White
  , piece: Nothing
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
square { sq, coordinates, color, piece } =
  HH.div
    (pure color' <> pure (HP.classes (coordinatesToCSS coordinates)))
    [ Piece.pieceHTML piece
    ]
  where
  color' = CSS.style $ case color of
    Black ->
      Background.backgroundColor $ CSS.Color.lighten 0.6 CSS.Color.black
    White ->
      Background.backgroundColor $ CSS.Color.white

handleAction ::
     forall o m. MonadAff m
  => Sq
  -> Action
  -> H.HalogenM State Action ChildSlots o m Unit
handleAction sq = case _ of
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

    pure unit

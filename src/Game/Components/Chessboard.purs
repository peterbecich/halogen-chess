module Game.Components.Chessboard where

import Prelude

import Data.Array (length)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)
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
import Affjax.RequestBody as RequestBody
import Affjax (Error, Response, get, post)
import Affjax.ResponseFormat (json, string)
import Data.Either (Either(Left, Right))
import Data.Traversable (for_)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Options (defaultOptions)
import Game.Chess.Internal.Square (Sq)
import Game.Chess.Board (Board(Board), _Board)
import Game.Chess.Move (Move(Move))
import Halogen.Store.Monad as Store
import Game.Store as GS
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectAll)
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
  , fenPosition     :: String
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

_chessboard :: Proxy "chessboard"
_chessboard = Proxy

component ::
     forall q i o m
   . MonadAff m
  => MonadStore GS.Action GS.Store m
  => H.Component q i o m
component = connect selectAll $ H.mkComponent
  { initialState: deriveState
  , render
  , eval: H.mkEval
    $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

deriveState :: forall i. Connected GS.Store i -> State
deriveState { context } =
  { toggleCount: 0
  , buttonState: Nothing
  , board: context.board
  , sourceSelection: Nothing
  , destinationSelection: Nothing
  , fenPosition: mempty
  }

initialState :: forall i. i -> State
initialState _ =
  { toggleCount: 0
  , buttonState: Nothing
  , board: Board mempty
  , sourceSelection: Nothing
  , destinationSelection: Nothing
  , fenPosition: mempty
  }

render ::
     forall m. MonadAff m
  => State
  -> H.ComponentHTML Action ChildSlots m
render state = HH.div style (h <> [HH.text state.fenPosition])
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
     forall o m
   . MonadAff m
  => MonadStore GS.Action GS.Store m
  => Action
  -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  ReceiveSquare (Square.Clicked sq) -> do
    {sourceSelection, destinationSelection, fenPosition} <- H.get
    case Tuple sourceSelection destinationSelection of
      Tuple (Just from) Nothing -> do
        H.tell Square._square (Sq' sq) (pure Square.Select)
        H.modify_ _ { destinationSelection = Just sq }
        -- make move

        eMoveResponse :: Either Error (Response String) <-
          liftAff
            $ post string "/move"
            $ Just $ RequestBody.json
            $ encodeJson $ Move { fenPosition, from, to: sq }
        case eMoveResponse of
          Left _              -> logShow "no move response"
          Right moveResponse -> do
            void $ H.modify _ {fenPosition = moveResponse.body}
            logShow $ "got move: " <> moveResponse.body

      Tuple (Just src) (Just dst) -> do
        let
          srcSlot = Sq' src
          dstSlot = Sq' dst
        H.tell Square._square srcSlot (pure Square.Unselect)
        H.tell Square._square dstSlot (pure Square.Unselect)
        void $ H.modify _ { sourceSelection = Just sq, destinationSelection = Nothing }
        H.tell Square._square (Sq' sq) (pure Square.Select)

      Tuple Nothing (Just _) -> do
        H.modify_ _ { sourceSelection = Nothing, destinationSelection = Nothing }
      Tuple Nothing Nothing -> do
        H.tell Square._square (Sq' sq) (pure Square.Select)
        H.modify_ _ { sourceSelection = Just sq }

  Initialize -> do
    logShow "Initialize"

    s <- H.get
    logShow $ "Board: " <> show (length (view _Board s.board))
    when (length (view _Board s.board) == 0) do
      logShow "Board has 0 items"
      eBoardResponse :: Either Error (Response Json) <-
        liftAff $ get json "/board"
      case eBoardResponse of
        Left _              -> logShow "no board response"
        Right boardResponse ->
          for_ (genericDecodeAeson defaultOptions boardResponse.body)
            $ \(board :: Board) -> do
              logShow "got board"
              Store.updateStore $ GS.SetBoard board
              H.modify _ {board = board}
      eStartPositionResponse :: Either Error (Response String) <-
        liftAff $ get string "/start"
      case eStartPositionResponse of
        Left _              -> logShow "no start position response"
        Right startResponse -> do
          void $ H.modify _ {fenPosition = startResponse.body}
          logShow $ "got start pos: " <> startResponse.body

  CheckButtonState -> pure unit

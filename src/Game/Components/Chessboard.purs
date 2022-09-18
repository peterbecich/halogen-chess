module Game.Components.Chessboard where

import Prelude

import Data.Array (length)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Class (encodeJson)
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import Effect.Class.Console (logShow)
import Effect.Aff.Class (class MonadAff, liftAff)
import Affjax.RequestBody as RequestBody
import Affjax.Web (Error, Response, get, post)
import Affjax.StatusCode (StatusCode(StatusCode))
import Affjax.ResponseFormat (json, string)
import Data.Either (Either(Left, Right))
import Halogen.Store.Monad (class MonadStore)
import Data.Traversable (for_)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Options (defaultOptions)
import Game.Sq (Sq'(Sq'))
import Game.Chess.Internal.Square (Sq)
import Game.Chess.Board (Board(Board), _Board)
import Game.Chess.Move (Move(Move))
import Halogen.Store.Monad as Store
import Game.Store as GS
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Select (selectAll)
import Game.Components.Square as Square
import CSS as CSS
import Halogen.HTML.CSS as HCSS
import CSS.Display as CSS.Display


data Action
  = Initialize
  | ReceiveSquare Square.Output

type State =
  { toggleCount     :: Int
  , buttonState     :: Maybe Boolean
  , board           :: Board
  , sourceSelection :: Maybe Sq
  , destinationSelection :: Maybe Sq
  , fenPosition     :: String
  }

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
  , fenPosition: context.fenPosition
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

render
  :: forall m. MonadAff m
  => MonadStore GS.Action GS.Store m
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
          Right (moveResponse :: Response String) ->
            if (moveResponse.status == StatusCode 200)
            then do
              H.modify_ _ {fenPosition = moveResponse.body}
              Store.updateStore $ GS.SetFenPosition moveResponse.body
              mPiece <- H.request Square._square (Sq' from) Square.GivePiece
              for_ mPiece $ \piece -> do
                H.tell Square._square (Sq' sq) (pure $ Square.ReceivePiece piece)
            else logShow $ "illegal move"

      Tuple (Just src) (Just dst) -> do
        let
          srcSlot = Sq' src
          dstSlot = Sq' dst
        H.tell Square._square srcSlot (pure Square.Unselect)
        H.tell Square._square dstSlot (pure Square.Unselect)
        H.modify_ _ { sourceSelection = Just sq, destinationSelection = Nothing }
        H.tell Square._square (Sq' sq) (pure Square.Select)

      Tuple Nothing (Just _) -> do
        H.modify_ _ { sourceSelection = Nothing, destinationSelection = Nothing }
      Tuple Nothing Nothing -> do
        H.tell Square._square (Sq' sq) (pure Square.Select)
        H.modify_ _ { sourceSelection = Just sq }

  Initialize -> do
    logShow "Initialize"

    s <- H.get
    logShow $ "Board squares: " <> show (length (view _Board s.board))
    logShow $ "FEN position: " <> s.fenPosition
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
              H.modify_ _ {board = board}
      eStartPositionResponse :: Either Error (Response String) <-
        liftAff $ get string "/start"
      case eStartPositionResponse of
        Left _              -> logShow "no start position response"
        Right startResponse -> do
          void $ H.modify _ {fenPosition = startResponse.body}
          logShow $ "got start pos: " <> startResponse.body

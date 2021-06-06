module Game.Sq where

import Prelude

import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Game.Chess.Internal.Square (Sq)
import Data.Argonaut.Aeson.Options as Argonaut
import Data.Argonaut.Core (Json, stringify)
import Data.Hashable (class Hashable, hash)
import Data.Tuple (Tuple(Tuple))
import Game.Chess.Internal (Color(..), PieceType)
import Data.Maybe (Maybe(..))


type State =
  { sq :: Sq
  , coordinates :: Tuple Int Int
  , color :: Color
  , piece :: Maybe (Tuple Color PieceType)
  , selected :: Boolean
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
instance hashsq :: Hashable Sq' where
  hash sq = hash $ show sq

-- File auto generated by purescript-bridge! --
module Game.Chess.Board where

import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options as Argonaut
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Game.Chess.Internal.Square (Sq)
import Prim (Array)
import Type.Proxy (Proxy(Proxy))

import Prelude

newtype Board =
    Board (Array Sq)



instance encodeJsonBoard :: EncodeJson Board where
  encodeJson = genericEncodeAeson Argonaut.defaultOptions
instance decodeJsonBoard :: DecodeJson Board where
  decodeJson = genericDecodeAeson Argonaut.defaultOptions
derive instance genericBoard :: Generic Board _
derive instance newtypeBoard :: Newtype Board _

--------------------------------------------------------------------------------
_Board :: Iso' Board (Array Sq)
_Board = _Newtype
--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module MyLib (someFunc) where

-- https://docs.servant.dev/en/stable/cookbook/structuring-apis/StructuringApis.html
import Prelude

import           Control.Lens (view)
import           Control.Monad (forM, mapM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.Data
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Data.Typeable
import           Network.HTTP.Media ((//), (/:))
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.Gzip as Gzip
import           Servant
import           Servant.API (Accept (..))

import Game.Chess.Orphans

import Game.Chess
    (Color (..), PieceType (..), Sq (..), isLight, pieceAt, startpos, toRF)
import Game.Chess.Board (Board, allPieces)

instance Accept HTML where
  contentType _ = let
    p :: String -> BS.ByteString
    p = encodeUtf8 . Text.pack
    in p "text" // p "html" /: (p "charset", p "utf-8")

type ChessServer
  = "board" :> (Get '[JSON] Board)
  :<|> "rf"    :> ReqBody '[JSON] Sq :> Post '[JSON] (Int, Int)
  :<|> "color" :> ReqBody '[JSON] Sq :> Post '[JSON] Color
  :<|> "pieceAtStartingPosition" :> ReqBody '[JSON] Sq :> Post '[JSON] (Maybe (Color, PieceType))
  :<|> Raw

instance MimeRender HTML RawHtml where
    mimeRender _ =  unRaw

data HTML = HTML
newtype RawHtml = RawHtml { unRaw :: BSL.ByteString }

chessServer :: Server ChessServer
chessServer = return allPieces
  :<|> return . toRF
  :<|> (\sq -> return (if isLight sq then White else Black))
  :<|> (\sq -> return $ pieceAt startpos sq)
  :<|> serveDirectoryWebApp "static"

type RootServer = Get '[HTML] RawHtml
  :<|> "chess" :> Get '[HTML] RawHtml
  :<|> "pageB" :> Get '[HTML] RawHtml
  :<|> "pageC" :> Get '[HTML] RawHtml

rootServer :: BSL.ByteString -> Server RootServer
rootServer root = return (RawHtml root)
  :<|> return (RawHtml root)
  :<|> return (RawHtml root)
  :<|> return (RawHtml root)

api :: Proxy (RootServer :<|> ChessServer)
api = Proxy

someFunc :: IO ()
someFunc = do
  root <- liftIO $ BSL.readFile "static/index.html"
  run 8080
    . Gzip.gzip Gzip.def
    $ serve api (rootServer root :<|> chessServer)

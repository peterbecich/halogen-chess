{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module MyLib (someFunc) where

-- https://docs.servant.dev/en/stable/cookbook/structuring-apis/StructuringApis.html
import Prelude

import           Control.Lens (view)
import           Control.Monad (forM, mapM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.Data (Proxy (..))
import           Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Media ((//), (/:))
import           Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Middleware.Gzip as Gzip
import           Servant
    ( Accept (contentType)
    , Get
    , Handler
    , JSON
    , MimeRender (..)
    , PlainText
    , Post
    , Raw
    , ReqBody
    , Server
    , ServerError (errBody)
    , err403
    , serve
    , serveDirectoryWebApp
    , throwError
    , type (:<|>) (..)
    , type (:>)
    )
import           Servant.API (Accept (..))
import           System.Environment (lookupEnv)

import Game.Chess
    ( Color (..)
    , PieceType (..)
    , Position
    , Square (..)
    , fromFEN
    , isLight
    , pieceAt
    , startpos
    , toFEN
    , unRank
    , unFile
    , rank
    , file
    )
import Game.Chess.Board (Board, allPieces, checkMove')
import Game.Chess.Move (Move)

instance Accept HTML where
  contentType _ = let
    p :: String -> BS.ByteString
    p = encodeUtf8 . Text.pack
    in p "text" // p "html" /: (p "charset", p "utf-8")

type MoveServer
  = "move"  :> ReqBody '[JSON] Move :> Post '[PlainText] String

type ChessServer
  = "board" :> (Get '[JSON] Board)
  :<|> "rf"    :> ReqBody '[JSON] Square :> Post '[JSON] (Int, Int)
  :<|> "color" :> ReqBody '[JSON] Square :> Post '[JSON] Color
  :<|> "pieceAtStartingPosition"
       :> ReqBody '[JSON] Square
       :> Post '[JSON] (Maybe (Color, PieceType))
  :<|> "start" :> Get '[PlainText] String
  :<|> MoveServer
  :<|> Raw

instance MimeRender HTML RawHtml where
    mimeRender _ =  unRaw

data HTML = HTML
newtype RawHtml = RawHtml { unRaw :: BSL.ByteString }

moveServer :: Move -> Handler String
moveServer move = do
  liftIO $ putStrLn "Move"
  let mv = checkMove' move
  case mv of
    Nothing -> do
      liftIO $ putStrLn "Illegal chess move"
      throwError
        $ err403 { errBody = UTF8.fromString "illegal chess move" }
    Just position -> return position

chessServer :: FilePath -> Server ChessServer
chessServer clientDir = return allPieces
  :<|> (\sq -> return (unRank $ rank sq, unFile $ file sq))
  :<|> (\sq -> return (if isLight sq then White else Black))
  :<|> (return . pieceAt startpos)
  :<|> return (toFEN startpos)
  :<|> moveServer
  :<|> serveDirectoryWebApp clientDir

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
  mClientDir <- lookupEnv "CLIENT_DIR"
  let
    rootDir :: FilePath
    rootDir = fromMaybe "static" mClientDir
    rootFile :: FilePath
    rootFile = rootDir <> "/index.html"

  putStrLn $ "Root file: " <> rootFile
  root <- liftIO $ BSL.readFile rootFile
  putStrLn $ "Start pos: " <> toFEN startpos
  run 8080
    . Gzip.gzip Gzip.def
    $ serve api (rootServer root :<|> chessServer rootDir)

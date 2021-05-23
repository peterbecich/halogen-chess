module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Game.Components.Router (Query(Navigate), component)
import Routing.Hash (matchesWith)

import Game.Store as GS
import Halogen.Store.Monad (runStoreT)

import Game.Routes (Route(..), routeCodec)
import Routing.Duplex (parse, print)

main :: Effect Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  root <- runStoreT GS.initialStore GS.reduce component
  halogenIO <- runUI root unit body
  void $ H.liftEffect $ do
    H.liftEffect do
      log "waiting to match on one of these:"
      log $ print routeCodec PlayChess
      log $ print routeCodec PageB
      log $ print routeCodec PageC

    matchesWith (parse routeCodec) $ \_ new -> do
      case new of
        PlayChess -> H.liftEffect $ log "play chess"
        PageB -> H.liftEffect $ log "page B"
        PageC -> H.liftEffect $ log "page C"
      launchAff_ $ halogenIO.query $ H.mkTell $ Navigate new

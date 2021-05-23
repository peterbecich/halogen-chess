module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Game.Components.Router (Query(Navigate), component)
import Routing.PushState (matchesWith, makeInterface)
import Foreign (unsafeToForeign)
import Game.Store as GS
import Halogen.Store.Monad (runStoreT)

import Game.Routes (Route(..), routeCodec)
import Routing.Duplex (parse, print)

main :: Effect Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  nav <- H.liftEffect makeInterface

  root <- runStoreT GS.initialStore GS.reduce (component nav)

  halogenIO <- runUI root unit body
  void $ H.liftEffect $ do
    log "waiting to match on one of these:"
    log $ print routeCodec PlayChess
    log $ print routeCodec PageB
    log $ print routeCodec PageC

    -- nav.pushState (unsafeToForeign {}) "/chess"
    nav # matchesWith (parse routeCodec) \_ new -> do
      case new of
        PlayChess -> nav.pushState (unsafeToForeign {}) "/chess"
        PageB     -> nav.pushState (unsafeToForeign {}) "/pageB"
        PageC     -> nav.pushState (unsafeToForeign {}) "/pageC"
      launchAff_ $ halogenIO.query $ H.mkTell $ Navigate new

module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Aff.Driver (HalogenIO)
import Halogen.Component (Component)
import Halogen.VDom.Driver (runUI)
import Game.Components.Router (Query(Navigate), component)
import Routing.PushState (matchesWith, makeInterface)
import Game.Store as GS
import Halogen.Store.Monad (runStoreT)

import Game.Routes (Route(..), routeCodec)
import Routing.Duplex (parse, print)

main :: Effect Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  nav <- H.liftEffect makeInterface

  root  <- -- :: Component Query Int Int
    runStoreT GS.initialStore GS.reduce (component nav)

  halogenIO <- runUI root unit body

  void $ H.liftEffect $ do
    log "waiting to match on one of these:"
    log $ print routeCodec PlayChess
    log $ print routeCodec PageB
    log $ print routeCodec PageC
    nav # matchesWith (parse routeCodec) \_ new -> do
      launchAff_ $ void $ halogenIO.query $ H.mkTell $ Navigate new

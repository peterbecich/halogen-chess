module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Game.Chess (component)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  H.liftEffect $ log $ "Chess"
  void $ runUI component unit body

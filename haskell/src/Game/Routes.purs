module Game.Routes where

import Prelude

import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)

import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = PlayChess
  | PageB
  | PageC

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "PlayChess" : "chess" / noArgs
  , "PageB" : "pageB" / noArgs
  , "PageC" : "pageC" / noArgs
  }

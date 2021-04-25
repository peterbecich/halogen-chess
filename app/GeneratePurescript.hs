module Main where

import Control.Lens ()
import Data.Text (pack)
import Language.PureScript.Bridge (buildBridge, writePSTypesWith)
import Language.PureScript.Bridge.CodeGenSwitches
    (ForeignOptions (ForeignOptions), genForeign, useGenRep)

import Types.Bridge (myBridge, myTypes)

frontEndRoot :: String
frontEndRoot = "src"

main :: IO ()
main = do
  writePSTypesWith
    (genForeign (ForeignOptions False False))
    frontEndRoot
    (buildBridge myBridge)
    myTypes

module Main where

import Prelude

import Data.Gospel (parseInt)
import Effect (Effect)
import Effect.Console (log, logShow)

main :: Effect Unit
main = do
    logShow $ parseInt "3"
    logShow $ parseInt "345"
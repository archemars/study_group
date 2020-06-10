module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

foreign import consoleClear :: Effect Unit

main :: Effect Unit
main = do
  log "🍝"
  log "🍝"
  consoleClear
  log "🍝"
  log "🍝"

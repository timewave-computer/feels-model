module Main where

import Prelude
import UI (renderUI, initialState)
import Effect (Effect)
import Effect.Console (log)
import FFI (onDOMReady)

main :: Effect Unit
main = do
  log "Starting Feels Protocol Application"
  onDOMReady do
    log "DOM ready, initializing UI"
    renderUI initialState
-- Application entry point for the Feels Protocol DeFi system.
-- Initializes the web application, sets up the DOM, and renders the main UI.
-- This is the starting point for the unified lending-based financial protocol.
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import UI.App (renderUI)
import FFI (onDOMReady)

main :: Effect Unit
main = do
  log "Starting Feels Protocol"
  onDOMReady do
    log "DOM ready, initializing Halogen application"
    -- Render the Halogen UI (state initialization happens in component)
    renderUI
    log "Halogen application initialized successfully"
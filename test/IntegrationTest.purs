module Test.Integration where

import Prelude
import Effect (Effect)
import Effect.Console (log)

-- Integration tests are temporarily disabled during migration to vertical ticks architecture
-- TODO: Rewrite tests for new pool-centric system

runIntegrationTests :: Effect Unit
runIntegrationTests = do
  log "Integration tests temporarily disabled during migration"
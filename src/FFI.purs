-- Foreign Function Interface providing JavaScript interop for the protocol.
-- Bridges PureScript business logic with JavaScript runtime functionality including
-- DOM manipulation, math operations, system metrics, and browser APIs.
-- Enables the web-based interface for the Feels Protocol.
module FFI where

import Prelude
import Effect (Effect)
import Data.Nullable (Nullable)
import Web.DOM.Element (Element)
import Foreign (Foreign)

-- System state functions
foreign import totalStaked :: Effect Number
foreign import totalLiquidity :: Effect Number
foreign import utilizationRate :: Effect Number
foreign import volatility :: forall a. a -> a -> Effect Number

-- Utility functions
foreign import currentTime :: Effect Number
foreign import generateId :: Number -> Int
foreign import toNumber :: Int -> Number

-- DOM manipulation
foreign import setInnerHTML :: Element -> String -> Effect Unit
foreign import addEventListener :: Element -> String -> Effect Unit -> Effect Unit
foreign import removeAllEventListeners :: Element -> String -> Effect Unit
foreign import getValue :: Element -> Effect String
foreign import getTextContent :: Element -> Effect String
foreign import parseFloat :: String -> Number
foreign import floor :: Number -> Int
-- Note: In actual usage, these are typed specifically in each module
foreign import setGlobalState :: forall a. a -> Effect Unit
foreign import getGlobalState :: forall a. Effect a
foreign import setTimeout :: Effect Unit -> Int -> Effect Unit

-- Application initialization
foreign import onDOMReady :: Effect Unit -> Effect Unit

-- Math functions used throughout the protocol
foreign import log10 :: Number -> Number
foreign import sqrt :: Number -> Number
foreign import sin :: Number -> Number
foreign import cos :: Number -> Number
foreign import log :: Number -> Number
foreign import exp :: Number -> Number
foreign import pow :: Number -> Number -> Number
foreign import generateRecordId :: Effect Int

-- DOM attribute access
foreign import getAttribute :: String -> Element -> Effect String

-- Chart rendering
foreign import initializePriceChart :: Effect Unit
foreign import checkAndInitializeChart :: Effect Unit
foreign import setChartData :: forall a. a -> Effect Unit

-- Remote control functions
foreign import registerRemoteAction :: forall a. String -> (Foreign -> Effect a) -> Effect Unit
foreign import triggerUIAction :: String -> Effect Boolean

-- Selective DOM updates
foreign import getElementById :: String -> Effect (Nullable Element)
foreign import setElementInnerHTML :: String -> String -> Effect Unit
foreign import setElementAttribute :: String -> String -> String -> Effect Unit
foreign import removeElementAttribute :: String -> String -> Effect Unit

-- Math utility functions
foreign import unsafeToInt :: Number -> Int
foreign import unsafeToNumber :: Int -> Number
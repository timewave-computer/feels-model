module FFI where

import Prelude
import Effect (Effect)
import Data.Unit (Unit)
import Web.DOM.Element (Element)

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
foreign import parseFloat :: String -> Number
foreign import floor :: Number -> Int
-- Note: In actual usage, these are typed specifically in each module
foreign import setGlobalState :: forall a. a -> Effect Unit
foreign import getGlobalState :: forall a. Effect a
foreign import setTimeout :: Effect Unit -> Int -> Effect Unit

-- Application initialization
foreign import onDOMReady :: Effect Unit -> Effect Unit

-- Math functions
foreign import log10 :: Number -> Number
foreign import generatePositionId :: Effect Int
-- | Generic Registry Utilities for Feels Protocol UI
-- |
-- | This module provides standardized registry patterns and utilities
-- | to reduce code duplication across AccountRegistry, TokenRegistry, and PoolRegistry.
module UI.Util.Registry
  ( -- Generic registry types
    GenericRegistry
  , RegistryConfig
  , RegistryQuery
  , RegistryUpdate
  -- Registry creation and initialization
  , createRegistry
  , initializeRegistry
  , withDefaultItems
  -- Common registry operations
  , registerItem
  , unregisterItem
  , getItem
  , getAllItems
  , getItemsBy
  , updateItem
  , hasItem
  , getItemCount
  -- Bulk operations
  , registerItems
  , updateItems
  , filterItems
  , mapItems
  -- Registry queries and utilities
  , findItem
  , findItems
  , existsItem
  , validateRegistry
  -- Registry state management
  , cloneRegistry
  , mergeRegistries
  , clearRegistry
  -- Configuration types
  , RegistryValidator
  , RegistryFilter
  , RegistryTransform
  ) where

import Prelude
import Data.Array (filter, length, any, all, (:), concat, find, sortBy, drop, take, fromFoldable) as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (traverse, traverse_)
import Data.Foldable (foldl, foldMap, foldr)
import Data.List (List)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_, modify)
import Control.Monad (when)

--------------------------------------------------------------------------------
-- Core Registry Types
--------------------------------------------------------------------------------

-- | Generic registry that can hold any type of items with string keys
type GenericRegistry a = Ref (Map String a)

-- | Configuration for registry behavior
type RegistryConfig a =
  { validator :: RegistryValidator a
  , keyExtractor :: a -> String
  , defaultItems :: Array a
  , allowDuplicates :: Boolean
  , maxItems :: Maybe Int
  }

-- | Query configuration for registry operations
type RegistryQuery a =
  { filter :: RegistryFilter a
  , sortBy :: Maybe (a -> a -> Ordering)
  , limit :: Maybe Int
  , offset :: Int
  }

-- | Update configuration for bulk operations
type RegistryUpdate a =
  { validator :: RegistryValidator a
  , onError :: String -> Effect Unit
  , onSuccess :: a -> Effect Unit
  }

-- | Function type for validating registry items
type RegistryValidator a = a -> Either String a

-- | Function type for filtering registry items
type RegistryFilter a = a -> Boolean

-- | Function type for transforming registry items
type RegistryTransform a b = a -> b

--------------------------------------------------------------------------------
-- Default Configurations
--------------------------------------------------------------------------------

-- | Default registry configuration
defaultRegistryConfig :: forall a. RegistryConfig a
defaultRegistryConfig =
  { validator: Right
  , keyExtractor: const "default"
  , defaultItems: []
  , allowDuplicates: true
  , maxItems: Nothing
  }

-- | Default query configuration
defaultRegistryQuery :: forall a. RegistryQuery a
defaultRegistryQuery =
  { filter: const true
  , sortBy: Nothing
  , limit: Nothing
  , offset: 0
  }

-- | Default update configuration
defaultRegistryUpdate :: forall a. RegistryUpdate a
defaultRegistryUpdate =
  { validator: Right
  , onError: const (pure unit)
  , onSuccess: const (pure unit)
  }

-- | No-op validator that accepts all items
noValidation :: forall a. RegistryValidator a
noValidation = Right

-- | No filter - includes all items
noFilter :: forall a. RegistryFilter a
noFilter = const true

--------------------------------------------------------------------------------
-- Registry Creation and Initialization
--------------------------------------------------------------------------------

-- | Create a new empty registry
createRegistry :: forall a. Effect (GenericRegistry a)
createRegistry = new Map.empty

-- | Initialize registry with configuration
initializeRegistry :: forall a. RegistryConfig a -> Effect (GenericRegistry a)
initializeRegistry config = do
  registry <- createRegistry
  let items = config.defaultItems
  when (Array.length items > 0) $ do
    _ <- registerItems registry items config.validator
    pure unit
  pure registry

-- | Helper to create registry with default items
withDefaultItems :: forall a. Array a -> (a -> String) -> Effect (GenericRegistry a)
withDefaultItems items keyExtractor = do
  registry <- createRegistry
  traverse_ (\item -> registerItem registry (keyExtractor item) item noValidation) items
  pure registry

--------------------------------------------------------------------------------
-- Core Registry Operations
--------------------------------------------------------------------------------

-- | Register an item in the registry
registerItem :: forall a. GenericRegistry a -> String -> a -> RegistryValidator a -> Effect (Either String Unit)
registerItem registry key item validator =
  case validator item of
    Left err -> pure (Left err)
    Right validItem -> do
      modify_ (Map.insert key validItem) registry
      pure (Right unit)

-- | Remove an item from the registry
unregisterItem :: forall a. GenericRegistry a -> String -> Effect Boolean
unregisterItem registry key = do
  currentMap <- read registry
  case Map.lookup key currentMap of
    Nothing -> pure false
    Just _ -> do
      modify_ (Map.delete key) registry
      pure true

-- | Get a specific item from the registry
getItem :: forall a. GenericRegistry a -> String -> Effect (Maybe a)
getItem registry key = do
  registryMap <- read registry
  pure (Map.lookup key registryMap)

-- | Get all items from the registry
getAllItems :: forall a. GenericRegistry a -> Effect (Array a)
getAllItems registry = do
  registryMap <- read registry
  pure (Array.fromFoldable (Map.values registryMap))

-- | Get items matching a filter
getItemsBy :: forall a. GenericRegistry a -> RegistryFilter a -> Effect (Array a)
getItemsBy registry filterFn = do
  registryMap <- read registry
  pure (Array.fromFoldable (Map.values (Map.filter filterFn registryMap)))

-- | Update an existing item in the registry
updateItem :: forall a. GenericRegistry a -> String -> (a -> a) -> Effect Boolean
updateItem registry key updateFn = do
  currentMap <- read registry
  case Map.lookup key currentMap of
    Nothing -> pure false
    Just item -> do
      let updatedItem = updateFn item
      modify_ (Map.insert key updatedItem) registry
      pure true

-- | Check if an item exists in the registry
hasItem :: forall a. GenericRegistry a -> String -> Effect Boolean
hasItem registry key = do
  registryMap <- read registry
  pure (Map.member key registryMap)

-- | Get the total number of items in the registry
getItemCount :: forall a. GenericRegistry a -> Effect Int
getItemCount registry = do
  registryMap <- read registry
  pure (Map.size registryMap)

--------------------------------------------------------------------------------
-- Bulk Operations
--------------------------------------------------------------------------------

-- | Register multiple items at once
registerItems :: forall a. GenericRegistry a -> Array a -> RegistryValidator a -> Effect (Array (Either String Unit))
registerItems registry items validator = do
  traverse (\item -> 
    case validator item of
      Left err -> pure (Left err)
      Right validItem -> do
        let key = show (Array.length items) -- Simple key generation
        registerItem registry key validItem noValidation
  ) items

-- | Update multiple items based on a condition
updateItems :: forall a. GenericRegistry a -> RegistryFilter a -> (a -> a) -> Effect Int
updateItems registry filterFn updateFn = do
  registryMap <- read registry
  let filteredEntries = Map.toUnfoldable registryMap # Array.filter (\(Tuple _ item) -> filterFn item)
      updateEntry (Tuple key item) = Tuple key (updateFn item)
      updatedEntries = map updateEntry filteredEntries
  
  traverse_ (\(Tuple key updatedItem) -> 
    modify_ (Map.insert key updatedItem) registry
  ) updatedEntries
  
  pure (Array.length updatedEntries)

-- | Filter items in the registry (destructive operation)
filterItems :: forall a. GenericRegistry a -> RegistryFilter a -> Effect Int
filterItems registry filterFn = do
  registryMap <- read registry
  let filteredMap = Map.filter filterFn registryMap
  write filteredMap registry
  pure (Map.size filteredMap)

-- | Transform all items in the registry
mapItems :: forall a b. GenericRegistry a -> RegistryTransform a b -> Effect (Array b)
mapItems registry transformFn = do
  allItems <- getAllItems registry
  pure (map transformFn allItems)

--------------------------------------------------------------------------------
-- Registry Queries and Utilities
--------------------------------------------------------------------------------

-- | Find the first item matching a predicate
findItem :: forall a. GenericRegistry a -> RegistryFilter a -> Effect (Maybe a)
findItem registry predicate = do
  allItems <- getAllItems registry
  pure (findInArray predicate allItems)
  where
    findInArray :: RegistryFilter a -> Array a -> Maybe a
    findInArray = Array.find

-- | Find all items matching a predicate with query configuration
findItems :: forall a. GenericRegistry a -> RegistryQuery a -> Effect (Array a)
findItems registry query = do
  allItems <- getAllItems registry
  let filteredItems = Array.filter query.filter allItems
      sortedItems = case query.sortBy of
        Nothing -> filteredItems
        Just compareFn -> sortByComparison compareFn filteredItems
      offsetItems = drop query.offset sortedItems
      limitedItems = case query.limit of
        Nothing -> offsetItems
        Just limit -> take limit offsetItems
  pure limitedItems
  where
    sortByComparison :: (a -> a -> Ordering) -> Array a -> Array a
    sortByComparison _ [] = []
    sortByComparison _ [x] = [x]
    sortByComparison cmp xs = sortBy cmp xs -- Simplified sort
    
    sortBy :: (a -> a -> Ordering) -> Array a -> Array a
    sortBy = Array.sortBy

    drop :: Int -> Array a -> Array a
    drop = Array.drop

    take :: Int -> Array a -> Array a
    take = Array.take

-- | Check if any item exists matching a predicate
existsItem :: forall a. GenericRegistry a -> RegistryFilter a -> Effect Boolean
existsItem registry predicate = do
  allItems <- getAllItems registry
  pure (Array.any predicate allItems)

-- | Validate all items in the registry
validateRegistry :: forall a. GenericRegistry a -> RegistryValidator a -> Effect (Array String)
validateRegistry registry validator = do
  allItems <- getAllItems registry
  let validationResults = map validator allItems
      errors = validationResults # Array.filter isLeft # map extractError
  pure errors
  where
    isLeft (Left _) = true
    isLeft (Right _) = false
    
    extractError (Left err) = err
    extractError (Right _) = ""

--------------------------------------------------------------------------------
-- Registry State Management
--------------------------------------------------------------------------------

-- | Create a copy of the registry
cloneRegistry :: forall a. GenericRegistry a -> Effect (GenericRegistry a)
cloneRegistry registry = do
  registryMap <- read registry
  new registryMap

-- | Merge two registries (second registry wins on conflicts)
mergeRegistries :: forall a. GenericRegistry a -> GenericRegistry a -> Effect (GenericRegistry a)
mergeRegistries registry1 registry2 = do
  map1 <- read registry1
  map2 <- read registry2
  let mergedMap = Map.union map2 map1  -- map2 wins on conflicts
  new mergedMap

-- | Clear all items from the registry
clearRegistry :: forall a. GenericRegistry a -> Effect Unit
clearRegistry registry = write Map.empty registry

--------------------------------------------------------------------------------
-- Specialized Registry Helpers
--------------------------------------------------------------------------------

-- | Create a registry for tokens with validation
createTokenRegistry :: forall a. Array a -> (a -> String) -> RegistryValidator a -> Effect (GenericRegistry a)
createTokenRegistry defaultTokens keyExtractor validator = do
  let config = defaultRegistryConfig 
        { defaultItems = defaultTokens
        , keyExtractor = keyExtractor
        , validator = validator
        }
  initializeRegistry config

-- | Create a registry for pools
createPoolRegistry :: forall a. Array a -> (a -> String) -> Effect (GenericRegistry a)
createPoolRegistry defaultPools keyExtractor = 
  withDefaultItems defaultPools keyExtractor

-- | Create a registry for accounts  
createAccountRegistry :: forall a. Array a -> (a -> String) -> Effect (GenericRegistry a)
createAccountRegistry defaultAccounts keyExtractor = 
  withDefaultItems defaultAccounts keyExtractor
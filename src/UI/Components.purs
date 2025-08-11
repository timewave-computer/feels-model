-- UI Components for the Feels Protocol application
-- Contains all render functions for different panels and UI elements
module UI.Components
  ( renderSystemPanel
  , renderGatewayPanel
  , renderWalletPanel
  , renderTokenCreatorPanel
  , renderUserTokensPanel
  , renderCreatePositionPanel
  , renderLoanBookPanel
  , renderPositionsPanel
  ) where

import Prelude
import Data.Array (length, null)
import Data.String.Common (trim)
import Data.String as String
import Data.Number as Number
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- Import UI state and actions from State module
import UI.State (UIState, Action(..), parseTokenType)
import Token (TokenMetadata)
import Position (Position, TermCommitment(..))
import Utils (formatAmount)

--------------------------------------------------------------------------------
-- System Panel Components
--------------------------------------------------------------------------------

-- System metrics panel
renderSystemPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderSystemPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "System Metrics" ]
    , case state.protocolStats of
        Nothing -> HH.div_ [ HH.text "Loading metrics..." ]
        Just stats -> HH.div
          [ HP.class_ (HH.ClassName "form-fields") ]
          [ renderMetric "Total Value Locked" $ formatAmount stats.totalValueLocked
          , renderMetric "FeelsSOL Supply" $ formatAmount stats.feelsSOLSupply
          , renderMetric "JitoSOL Locked" $ formatAmount stats.jitoSOLLocked
          , renderMetric "POL Balance" $ formatAmount stats.polBalance
          , renderMetric "Active Users" $ show stats.totalUsers
          , renderMetric "Active Positions" $ show stats.activePositions
          , renderMetric "Lender Offers" $ show stats.totalLenderOffers
          , renderMetric "Live Tokens" $ show stats.liveTokens
          ]
    ]
  where
    renderMetric label value =
      HH.div
        [ HP.class_ (HH.ClassName "list-item__content") ]
        [ HH.div [ HP.class_ (HH.ClassName "label") ] [ HH.text label ]
        , HH.div [ HP.class_ (HH.ClassName "value") ] [ HH.text value ]
        ]

--------------------------------------------------------------------------------
-- Gateway Panel Components
--------------------------------------------------------------------------------

-- Gateway panel (always visible)
renderGatewayPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderGatewayPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Gateway" ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ HH.label [ HP.for "jitosol-amount" ] [ HH.text "JitoSOL Amount (for entering):" ]
        , HH.input
            [ HP.type_ HP.InputNumber
            , HP.id "jitosol-amount"
            , HP.name "jitosol-amount"
            , HP.value (formatAmount state.jitoSOLAmount)
            , HE.onValueChange \v -> UpdateJitoSOLAmount (fromMaybe 0.0 (Number.fromString v))
            , HP.class_ (HH.ClassName "form__input")
            ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ HH.label [ HP.for "feelssol-amount" ] [ HH.text "FeelsSOL Amount (for exiting):" ]
        , HH.input
            [ HP.type_ HP.InputNumber
            , HP.id "feelssol-amount"
            , HP.name "feelssol-amount"
            , HP.value (formatAmount state.feelsSOLAmount)
            , HE.onValueChange \v -> UpdateFeelsSOLAmount (fromMaybe 0.0 (Number.fromString v))
            , HP.class_ (HH.ClassName "form__input")
            ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "gateway-actions") ]
        [ HH.button
            [ HE.onClick \_ -> EnterGateway
            , HP.class_ (HH.ClassName "btn btn--primary")
            ]
            [ HH.text "JitoSOL →", HH.br_, HH.text "FeelsSOL" ]
        , HH.button
            [ HE.onClick \_ -> ExitGateway
            , HP.class_ (HH.ClassName "btn btn--primary")
            ]
            [ HH.text "FeelsSOL →", HH.br_, HH.text "JitoSOL" ]
        ]
    ]

--------------------------------------------------------------------------------
-- Wallet Panel Components
--------------------------------------------------------------------------------

-- Wallet panel
renderWalletPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderWalletPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Your Wallet" ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-fields") ]
        [ renderBalance "JitoSOL" state.jitoSOLBalance
        , renderBalance "FeelsSOL" state.feelsSOLBalance
        ]
    ]
  where
    renderBalance label amount =
      HH.div
        [ HP.class_ (HH.ClassName "list-item__content") ]
        [ HH.div [ HP.class_ (HH.ClassName "label") ] [ HH.text label ]
        , HH.div [ HP.class_ (HH.ClassName "value") ] [ HH.text $ formatAmount amount ]
        ]

--------------------------------------------------------------------------------
-- Token Creation Panel Components
--------------------------------------------------------------------------------

-- Token creator panel (always visible)
renderTokenCreatorPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderTokenCreatorPanel state =
  let 
    hasErrors = not (null state.tokenValidationErrors)
    buttonClass = if hasErrors || trim state.tokenTicker == "" || trim state.tokenName == ""
                 then "btn btn--primary btn--disabled"
                 else "btn btn--primary"
  in
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Create Feels Token" ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ HH.label [ HP.for "token-ticker" ] [ HH.text "Token Ticker:" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.id "token-ticker"
            , HP.name "token-ticker"
            , HP.placeholder "e.g., ALPHA"
            , HP.class_ (HH.ClassName "form__input")
            , HP.value state.tokenTicker
            , HE.onValueChange \v -> UpdateTokenTicker (trim v)
            ]
        , HH.label [ HP.for "token-name" ] [ HH.text "Token Name:" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.id "token-name"
            , HP.name "token-name"
            , HP.placeholder "e.g., Alpha Protocol Token"
            , HP.class_ (HH.ClassName "form__input")
            , HP.value state.tokenName
            , HE.onValueChange \v -> UpdateTokenName (trim v)
            ]
        , renderValidationWarnings state.tokenValidationErrors
        , HH.button
            [ HE.onClick \_ -> CreateTokenUI
            , HP.class_ (HH.ClassName buttonClass)
            , HP.disabled (hasErrors || trim state.tokenTicker == "" || trim state.tokenName == "")
            ]
            [ HH.text "Create Token" ]
        ]
    , HH.p
        [ HP.class_ (HH.ClassName "info-text") ]
        [ HH.text "Tokens launch through the batch auction system" ]
    ]
  where
    renderValidationWarnings :: forall n. Array String -> H.ComponentHTML Action () n
    renderValidationWarnings errors =
      if null errors
        then HH.text ""
        else HH.div
          [ HP.class_ (HH.ClassName "validation-warnings") ]
          (map renderWarning errors)
    
    renderWarning :: forall n. String -> H.ComponentHTML Action () n  
    renderWarning error =
      HH.div
        [ HP.class_ (HH.ClassName "warning-message") ]
        [ HH.text error ]

--------------------------------------------------------------------------------
-- User Tokens Panel Components
--------------------------------------------------------------------------------

-- User tokens panel
renderUserTokensPanel :: forall m. Array TokenMetadata -> H.ComponentHTML Action () m
renderUserTokensPanel tokens =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Your Created Tokens" ]
    , if length tokens == 0
      then HH.p_ [ HH.text "No tokens created yet" ]
      else HH.div
        [ HP.class_ (HH.ClassName "token-list") ]
        (map renderToken tokens)
    ]
  where
    renderToken token =
      HH.div
        [ HP.class_ (HH.ClassName "list-item") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "list-item__header") ]
            [ HH.strong_ [ HH.text token.ticker ]
            , HH.text $ " " <> token.name
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "token-status") ]
            [ if token.live
              then HH.span 
                [ HP.class_ (HH.ClassName "status-live") ] 
                [ HH.text "Live" ]
              else HH.span 
                [ HP.class_ (HH.ClassName "status-pending") ] 
                [ HH.text "Pending" ]
            ]
        ]

--------------------------------------------------------------------------------
-- Position Creation Panel Components
--------------------------------------------------------------------------------

-- Create position panel
renderCreatePositionPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderCreatePositionPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Create Position" ]
    , -- Position form
      HH.div
        [ HP.class_ (HH.ClassName "position-form") ]
        [ -- Amount input
          HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "amount" ] [ HH.text "Amount:" ]
            , HH.input
                [ HP.type_ HP.InputNumber
                , HP.id "amount"
                , HP.name "amount"
                , HP.value (formatAmount state.inputAmount)
                , HE.onValueChange \v -> UpdateInputAmount (fromMaybe 0.0 (Number.fromString v))
                , HP.class_ (HH.ClassName "form__input")
                ]
            ]
        , -- Collateral selector (moved above asset)
          HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "collateral-asset" ] [ HH.text "Input Asset:" ]
            , HH.select
                [ HP.id "collateral-asset"
                , HP.name "collateral-asset"
                , HE.onValueChange \v -> SelectCollateralAsset (parseTokenType v)
                , HP.class_ (HH.ClassName "form__select")
                ]
                ([ HH.option [ HP.value "JitoSOL" ] [ HH.text "JitoSOL" ]
                 , HH.option [ HP.value "FeelsSOL" ] [ HH.text "FeelsSOL" ]
                 ] <> map (\token -> HH.option [ HP.value token.ticker ] [ HH.text token.ticker ]) state.userTokens)
            ]
        , -- Asset selector (moved below collateral)
          HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "output-asset" ] [ HH.text "Output Asset:" ]
            , HH.select
                [ HP.id "output-asset"
                , HP.name "output-asset"
                , HE.onValueChange \v -> SelectAsset (parseTokenType v)
                , HP.class_ (HH.ClassName "form__select")
                ]
                ([ HH.option [ HP.value "FeelsSOL" ] [ HH.text "FeelsSOL" ]
                 , HH.option [ HP.value "JitoSOL" ] [ HH.text "JitoSOL" ]
                 ] <> map (\token -> HH.option [ HP.value token.ticker ] [ HH.text token.ticker ]) state.userTokens)
            ]
        , -- Staking options
          renderLendOptions state
        , -- Create button
          HH.button
            [ HE.onClick \_ -> CreatePosition
            , HP.class_ (HH.ClassName "btn btn--primary btn--large")
            ]
            [ HH.text "Create Position" ]
        ]
    ]
  where
    renderLendOptions _state' =
      HH.div_
        [ -- Unbonding period
          HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "unbonding-period" ] [ HH.text "Unbonding Period:" ]
            , HH.select
                [ HP.id "unbonding-period"
                , HP.name "unbonding-period"
                , HE.onValueChange SetTermType
                , HP.class_ (HH.ClassName "form__select")
                ]
                [ HH.option [ HP.value "spot", HP.selected true ] [ HH.text "Spot (No Lock)" ]
                , HH.option [ HP.value "hourly" ] [ HH.text "Hourly" ]
                , HH.option [ HP.value "daily" ] [ HH.text "Daily" ]
                , HH.option [ HP.value "weekly" ] [ HH.text "Weekly" ]
                ]
            ]
        ]

--------------------------------------------------------------------------------
-- Lending Book Panel Components
--------------------------------------------------------------------------------

-- Loan book panel (shows available positions/offers)
renderLoanBookPanel :: forall m. Array Position -> H.ComponentHTML Action () m
renderLoanBookPanel offers =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Available Offers" ]
    , if length offers == 0
      then HH.p_ [ HH.text "No lending offers available" ]
      else HH.div
        [ HP.class_ (HH.ClassName "offer-list") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "offer__header") ]
            [ HH.div_ [ HH.text "ID" ]
            , HH.div_ [ HH.text "Lender" ]
            , HH.div_ [ HH.text "Asset" ]
            , HH.div_ [ HH.text "Amount (Avail/Total)" ]
            , HH.div_ [ HH.text "Collateral Required" ]
            , HH.div_ [ HH.text "Terms" ]
            , HH.div_ [ HH.text "Status" ]
            ]
        , HH.div_ (map renderOffer offers)
        ]
    ]
  where
    renderOffer offer =
      HH.div
        [ HP.class_ (HH.ClassName "offer__item") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "offer__id") ]
            [ HH.text $ "#" <> show offer.id ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__owner") ]
            [ HH.text $ formatAddress offer.owner ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__asset") ]
            [ HH.text $ show offer.tranche <> " tranche" ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__amount") ]
            [ HH.text $ formatAmount offer.amount <> " shares" ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__collateral") ]
            [ HH.text $ "Locked: " <> formatAmount offer.lockedAmount ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__terms") ]
            [ HH.text $ formatPositionTerms offer ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__status") ]
            [ renderOfferStatus offer ]
        ]

    formatPositionTerms position = 
      case position.term of
        Spot -> "Spot"
        Hourly hrs -> "Hourly (exp: " <> show hrs <> ")"
        Daily days -> "Daily (exp: " <> show days <> ")"
        Weekly weeks -> "Weekly (exp: " <> show weeks <> ")"

    formatAddress addr = 
      if String.length addr > 12
        then String.take 6 addr <> "..." <> String.drop (String.length addr - 4) addr
        else addr
    
    _formatCollateral offer = 
      -- Show the locked amount for position
      formatAmount offer.lockedAmount
    
    renderOfferStatus offer =
      let available = offer.amount - offer.lockedAmount
          total = offer.amount
          percentFilled = if total > 0.0 
                           then (total - available) / total * 100.0
                           else 0.0
      in if available <= 0.0
           then HH.span 
                  [ HP.class_ (HH.ClassName "status-filled") ] 
                  [ HH.text "Filled" ]
           else if percentFilled > 0.0
                  then HH.span 
                         [ HP.class_ (HH.ClassName "status-partial") ] 
                         [ HH.text $ show (Int.floor percentFilled) <> "% Filled" ]
                  else HH.span 
                         [ HP.class_ (HH.ClassName "status-available") ] 
                         [ HH.text "Available" ]

--------------------------------------------------------------------------------
-- Positions Panel Components
--------------------------------------------------------------------------------

-- Positions panel
renderPositionsPanel :: forall m. Array Position -> H.ComponentHTML Action () m
renderPositionsPanel positions =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Your Created Positions" ]
    , if length positions == 0
      then HH.p_ [ HH.text "No active positions" ]
      else HH.div
        [ HP.class_ (HH.ClassName "position-list") ]
        (map renderPosition positions)
    ]
  where
    renderPosition pos =
      HH.div
        [ HP.class_ (HH.ClassName "list-item") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "list-item__header") ]
            [ HH.text $ "Position #" <> show pos.id
            , HH.span
                [ HP.class_ (HH.ClassName "status status-active") ]
                [ HH.text "Active" ]
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "list-item__content") ]
            [ HH.div_ [ HH.text $ formatAmount pos.amount <> " " <> show pos.tranche ]
            , HH.div_ [ HH.text $ "Term: " <> formatPositionTerms pos ]
            , HH.div_ [ HH.text $ "Shares: " <> formatAmount pos.shares ]
            ]
        ]

    formatPositionTerms position = 
      case position.term of
        Spot -> "Spot"
        Hourly hrs -> "Hourly (exp: " <> show hrs <> ")"
        Daily days -> "Daily (exp: " <> show days <> ")"
        Weekly weeks -> "Weekly (exp: " <> show weeks <> ")"
    
    -- Legacy function no longer needed
  -- where
  --   renderPosition pos =
  --     HH.div
  --       [ HP.class_ (HH.ClassName "list-item") ]
  --       [ HH.div
  --           [ HP.class_ (HH.ClassName "list-item__header") ]
  --           [ HH.text $ "Position #" <> show pos.id <> " (" <> show pos.side <> ")"
  --           , HH.span
  --               [ HP.class_ (HH.ClassName $ "status " <> statusClass pos.status) ]
  --               [ HH.text $ show pos.status ]
  --           ]
  --       , HH.div
  --           [ HP.class_ (HH.ClassName "list-item__content") ]
  --           [ HH.div_ [ HH.text $ formatAmount pos.lendAmount <> " " <> show pos.lendAsset ]
  --           , HH.div_ [ HH.text $ "Collateral: " <> formatAmount pos.collateralAmount <> " " <> show pos.collateralAsset ]
  --           , HH.div_ [ HH.text $ "Terms: " <> formatTerms pos.terms ]
  --           ]
  --       ]
    
    -- formatTerms = case _ of
    --   SwapTerms -> "Swap"
    --   StakingTerms period -> "Staking (" <> show period <> ")"

    -- statusClass status = case status of
    --   _ -> "status-active"  -- Simplified for now

--------------------------------------------------------------------------------
-- Price Chart Panel Components
--------------------------------------------------------------------------------


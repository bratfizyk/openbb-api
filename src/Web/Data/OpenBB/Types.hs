{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|
  Module      : Web.Data.OpenBB.Types
  Description : Types for Client API wrapper for OpenBB-platform data.
  Copyright   : (c) Alojzy Leszcz, 2026
  License     : MIT
  Maintainer  : alojzy.leszcz.semester130@passinbox.com
  Stability   : experimental

  This module defines types for Servant-based client API to interact with the OpenBB-platform
-}
module Web.Data.OpenBB.Types where

import GHC.Generics (Generic)
import Data.Time (Day)

-- | A stock ticker symbol (e.g., "AAPL", "MSFT").
type Ticker = String

-- | A wrapper around date to prevent variations between providers
newtype ApiDay = ApiDay Day
  deriving (Eq, Ord, Show)

-- | The name of the data provider.
data Provider = YahooFinance | TMX
  deriving (Show, Read, Eq, Generic)

{-|
  Represents a single equity quote response from the OpenBB platform.
  Contains market data such as current price, volume, and intraday highs/lows.
-}
data EquityQuoteResponse = EquityQuoteResponse
  { symbol :: String
  , asset_type :: Maybe String
  , name :: Maybe String
  , currency :: Maybe String
  , last_price :: Maybe Double
  , prev_close :: Double
  , bid :: Maybe Double
  , ask :: Maybe Double
  , open :: Double
  , high :: Double
  , low :: Double
  , volume :: Maybe Int
  }
  deriving (Show, Generic)

{-|
  Represents a single equity quote response from the OpenBB platform.
  Contains market data such as current price, volume, and intraday highs/lows.
-}
data EquityHistoryResponse = EquityHistoryResponse
  { date :: ApiDay
  , open :: Double
  , high :: Double
  , low :: Double
  , close :: Double
  , volume :: Int
  }
  deriving (Show, Generic)

{-|
  A generic wrapper for API results containing a list of items.
  Used to structure the JSON response from the API endpoint.
-}
newtype ApiResult a = ApiResult
  {
    results :: [a]
  }
  deriving (Show, Generic)

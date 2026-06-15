{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
  Module      : Web.Data.OpenBB.API
  Description : Client API wrapper for OpenBB-platform equity price data.
  Copyright   : (c) Alojzy Leszcz, 2026
  License     : MIT
  Maintainer  : alojzy.leszcz.semester130@passinbox.com
  Stability   : experimental

  This module provides a Servant-based client API to interact with the OpenBB-platform
  for retrieving equity quote information. It supports querying by provider and symbol
  list, with built-in JSON parsing via Aeson.

  The default provider used is 'yfinance', but other providers supported by OpenBB
  can be specified explicitly.

  Example usage:

  @
  import Web.Data.OpenBB.API

  main :: IO ()
  main = do
    -- Fetch quotes for Apple and Google using default yfinance provider
    results <- runQuery (equityQuote ["AAPL", "GOOGL"])
    print results
@
-}
module Web.Data.OpenBB.API where

import Data.Aeson (FromJSON)
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API (ToHttpApiData(toUrlPiece), JSON, Required, QueryParam', type (:>), Get)
import Servant.Client (client, mkClientEnv, runClientM, ClientM, BaseUrl(BaseUrl), Scheme(Http))

-- | The name of the data provider (e.g., "yfinance", "polygon").
type Provider = String

-- | A stock ticker symbol (e.g., "AAPL", "MSFT").
type Ticker = String

instance ToHttpApiData [Ticker] where
  toUrlPiece :: [Ticker] -> Text
  toUrlPiece = pack . intercalate ","

{-|
  Represents a single equity quote response from the OpenBB platform.
  Contains market data such as current price, volume, and intraday highs/lows.
-}
data EquityQuoteResponse = EquityQuoteResponse
  { symbol :: String
  , asset_type :: String
  , name :: Maybe String
  , currency :: String
  , last_price :: Maybe Double
  , prev_close :: Double
  , bid :: Maybe Double
  , ask :: Maybe Double
  , open :: Double
  , high :: Double
  , low :: Double
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

instance FromJSON EquityQuoteResponse
instance FromJSON a => FromJSON (ApiResult a)

{-|
  Servant API definition for the equity price quote endpoint.
  Requires both a @provider@ and @symbol@ query parameters.
  Returns a JSON-wrapped list of 'EquityQuoteResponse'.
-}
type EquityAPI = "equity" :> "price" :> "quote" :> QueryParam' '[Required] "provider" Provider :> QueryParam' '[Required] "symbol" [Ticker] :> Get '[JSON] (ApiResult EquityQuoteResponse)


{-|
  Full API path prefix combined with 'EquityAPI'.
  Base path: @/api/v1/equity/price/quote@
-}
type API = "api" :> "v1" :> EquityAPI

{-|
  Proxy instance required by Servant to identify the API type at runtime.
-}
api :: Proxy API
api = Proxy

{-|
  Execute an equity quote request with a specific provider and list of tickers.
  Uses the Servant 'ClientM' monad.

  @
  -- Example: Query Tesla using the Polygon provider
  let res <- runQuery (equity "polygon" ["TSLA"])
  @
-}
equity :: Provider -> [Ticker] -> ClientM (ApiResult EquityQuoteResponse)
equity = client api

{-|
  Convenience function to fetch equity quotes using the default "yfinance" provider.
  Accepts a list of tickers and returns the raw 'ClientM' result.

  @param tickers List of stock symbols to query
  @
  equityQuote ["AAPL", "GOOGL"]
  @
-}
equityQuote :: [Ticker] -> ClientM (ApiResult EquityQuoteResponse)
equityQuote tickers = do equity "yfinance" tickers

{-|
  Runs a 'ClientM' request in the 'IO' monad.
  Sets up an HTTP manager and connects to localhost:6900 (typical OpenBB local gateway).
  Errors are printed and terminated; successful responses are returned.

  Note: This assumes the OpenBB platform is running locally on port 6900.
  If connecting to a remote API, modify the 'BaseUrl' scheme/host/port here.
-}
runQuery :: ClientM a -> IO a
runQuery req = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM req (mkClientEnv manager' (BaseUrl Http "localhost" 6900 ""))
  case res of
    Left err -> error $ "Error: " ++ show err
    Right eq -> return eq
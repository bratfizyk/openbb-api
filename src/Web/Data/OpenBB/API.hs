{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

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
    results <- runQuery (equityQuote YahooFinance ["AAPL", "GOOGL"])
    print results
@
-}
module Web.Data.OpenBB.API where

import Web.Data.OpenBB.Servant ()
import Web.Data.OpenBB.Types (Ticker, Provider(..), ApiResult, EquityQuoteResponse, EquityHistoryResponse)

import Data.Proxy (Proxy(..))
import Data.Time (Day)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API (JSON, Required, QueryParam, QueryParam', (:>), (:<|>) (..), Get)
import Servant.Client (client, mkClientEnv, runClientM, ClientM, BaseUrl(BaseUrl), Scheme(Http))

{-|
  Servant API definition for the equity price quote endpoint.
  Requires both a @provider@ and @symbol@ query parameters.
  Returns a JSON-wrapped list of 'EquityQuoteResponse'.
-}
type EquityAPI = 
  "equity" :> "price" :> (
    "quote"
      :> QueryParam' '[Required] "provider" Provider
      :> QueryParam' '[Required] "symbol" [Ticker]
      :> Get '[JSON] (ApiResult EquityQuoteResponse)
    :<|> "historical"
      :> QueryParam' '[Required] "provider" Provider
      :> QueryParam' '[Required] "symbol" Ticker
      :> QueryParam "start_date" Day
      :> Get '[JSON] (ApiResult EquityHistoryResponse)
  )

{-|
  Full API path prefix combined with 'EquityAPI'.
  Base path: @/api/v1/equity/price/quote@
-}
type API = "api" :> "v1" :> EquityAPI

{-|
  Convenience function to fetch equity quotes using the default "yfinance" provider.
  Accepts a list of tickers and returns the raw 'ClientM' result.

  @param tickers List of stock symbols to query
  @
    equityQuote YahooFinance ["AAPL", "GOOGL"]
  @
-}
equityQuote :: Provider -> [Ticker] -> ClientM (ApiResult EquityQuoteResponse)
{-|
  Convenience function to fetch equity history using the default "yfinance" provider.
  Accepts a list of tickers and returns the raw 'ClientM' result.

  @param tickers List of stock symbols to query
  @
    equityHistory YahooFinance "AAPL" "2026-10-01"
  @
-}
equityHistory :: Provider -> Ticker -> Maybe Day -> ClientM (ApiResult EquityHistoryResponse)
equityQuote :<|> equityHistory = client (Proxy :: Proxy API)

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
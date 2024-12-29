{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Web.Data.OpenBB.API where

import Data.Aeson (FromJSON)
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API (ToHttpApiData(toUrlPiece), JSON, Required, QueryParam', type (:>), Get)
import Servant.Client (client, mkClientEnv, runClientM, ClientM, BaseUrl(BaseUrl), Scheme(Http))

type Provider = String
type Ticker = String

instance ToHttpApiData [Ticker] where
  toUrlPiece :: [Ticker] -> Text
  toUrlPiece = pack . intercalate ","

data EquityQuoteResponse = EquityQuoteResponse
  { symbol :: String
  , asset_type :: String
  , name :: String
  , currency :: String
  , last_price :: Maybe Double
  , prev_close :: Double
  , open :: Double
  , high :: Double
  , low :: Double
  , volume :: Int
  }
  deriving (Show, Generic)

newtype ApiResult a = ApiResult
  {
    results :: [a]
  }
  deriving (Show, Generic)

instance FromJSON EquityQuoteResponse
instance FromJSON a => FromJSON (ApiResult a)

type EquityAPI = "equity" :> "price" :> "quote" :> QueryParam' '[Required] "provider" Provider :> QueryParam' '[Required] "symbol" [Ticker] :> Get '[JSON] (ApiResult EquityQuoteResponse)
type API = "api" :> "v1" :> EquityAPI

api :: Proxy API
api = Proxy

equity :: Provider -> [Ticker] -> ClientM (ApiResult EquityQuoteResponse)
equity = client api

equityQuote :: [Ticker] -> ClientM (ApiResult EquityQuoteResponse)
equityQuote tickers = do equity "yfinance" tickers

runQuery :: ClientM a -> IO a
runQuery req = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM req (mkClientEnv manager' (BaseUrl Http "localhost" 6900 ""))
  case res of
    Left err -> error $ "Error: " ++ show err
    Right eq -> return eq
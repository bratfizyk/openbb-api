{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Web.Data.OpenBB.Servant where

import Web.Data.OpenBB.Types (Ticker, Provider(..), ApiDay(..), ApiResult, EquityQuoteResponse, EquityHistoryResponse)

import Control.Applicative ((<|>))
import Data.Aeson ( withText, FromJSON(parseJSON)) 
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Servant.API (ToHttpApiData(toUrlPiece))

instance ToHttpApiData [Ticker] where
  toUrlPiece :: [Ticker] -> Text
  toUrlPiece = pack . intercalate ","

instance ToHttpApiData Provider where
  toUrlPiece :: Provider -> Text
  toUrlPiece YahooFinance = pack "yfinance"
  toUrlPiece TMX = pack "tmx"

instance FromJSON ApiDay where
  parseJSON = withText "date" $ \text ->
    case parseApiDay (unpack text) of
      Just day -> pure (ApiDay day)
      Nothing ->
        fail "Expected a date in yyyy-MM-dd or yyyy-MM-ddTHH:mm:ss format"

    where
      parseApiDay :: String -> Maybe Day
      parseApiDay value =
        parseTimeM True defaultTimeLocale "%Y-%m-%d" value
        <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" value

instance FromJSON EquityQuoteResponse
instance FromJSON EquityHistoryResponse
instance FromJSON a => FromJSON (ApiResult a)
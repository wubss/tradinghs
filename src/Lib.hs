{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Ticker, tickerFromJSON
    ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS

data Pair = ETHXBT | ETHGBP
    deriving (Show)

type Volume = Double
type Price = Double
data PriceInstance = PriceInstance Price Volume
    deriving (Show)

data Ticker = Ticker {
        ask :: PriceInstance
    } deriving (Show)

instance FromJSON Ticker where
  parseJSON = withObject "result" $ \o -> do
    --result <- o .: "result"
    pair <- o .: "XETHXXBT"
    [askPrice, askVol, askVol2] <- pair .: "a"
    let ask = PriceInstance askPrice askVol
    return Ticker {
        ask = ask
    }

tickerFromJSON :: BS.ByteString -> Maybe Ticker
tickerFromJSON s = 
    case o of
        Just a -> valueToTicker a
        Nothing -> Nothing
    where o = decode s

--valueToTicker :: Object -> Ticker
--valueToTicker (Object )
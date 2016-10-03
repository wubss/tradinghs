{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import GHC.Generics

import Network.Wreq

import Data.Maybe

import Control.Lens

import Data.Aeson
import Data.Map as Map

import qualified Data.ByteString.Lazy.Internal as BS

main :: IO ()
main = getKrakenPair "ETHXBT"

data Pair = ETHXBT | ETHGBP
    deriving (Show)

type Volume = Double
type Price = Double
data PriceInstance = PriceInstance Price Volume
    deriving (Show)

data Ticker = Ticker {
        ask :: PriceInstance,
        bid :: PriceInstance,
        last :: PriceInstance,
        volDay :: Volume,
        vol24 :: Volume,
        lowToday :: Price,
        low24 :: Price,
        highToday :: Price,
        high24 :: Price
    } deriving (Show)

instance FromJSON Ticker where
  parseJSON = withObject "result" $ \o -> do
    -- authorO :: Object
    result <- o .: "result"
    pair <- result .: "XETHXXBT"
    [askPrice, askVol, askVol2] <- pair .: "a"
    let ask = PriceInstance askPrice askVol
    -- And finally return the value.
    return Ticker {
        ask = ask
    }

getKrakenPair :: String -> IO ()
getKrakenPair pair = do
    requestBody <- makePublicGetRequest $ "Ticker?pair=" ++ pair
    print requestBody
    --let t = toTicker requestBody


makePublicGetRequest :: String -> IO (Maybe BS.ByteString)
makePublicGetRequest uri = do
    r <- makeGetRequest url
    let body = (r ^? responseBody)
    return body
        where url = krakenUrl "public/" ++ uri

makeGetRequest :: String -> IO (Response BS.ByteString)
makeGetRequest url = do
    r <- get url
    putStrLn $ "requesting" ++ url
    let status = (r ^. responseStatus . statusCode)
    putStrLn ("Response status code: " ++ show status)
    return r


krakenUrl :: String -> String
krakenUrl s = "https://api.kraken.com/0/" ++ s


--toTicker :: String -> Ticker

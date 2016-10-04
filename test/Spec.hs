{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe

import Data.Aeson
import Lib (Ticker, tickerFromJSON)

import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
    print $ showDecodedTicker getDecodedTicker

tickerResponseString = BS.pack "{\"error\":[],\"result\":{\"XETHXXBT\":{\"a\":[\"0.022000\",\"1\",\"1.000\"],\"b\":[\"0.021970\",\"315\",\"315.000\"],\"c\":[\"0.021970\",\"3.26201632\"],\"v\":[\"17948.30149634\",\"59492.21426846\"],\"p\":[\"0.021984\",\"0.021922\"],\"t\":[602,1770],\"l\":[\"0.021800\",\"0.021445\"],\"h\":[\"0.022110\",\"0.022110\"],\"o\":\"0.022006\"}}}"

getDecodedTicker :: Maybe Ticker
getDecodedTicker = tickerFromJSON tickerResponseString

showDecodedTicker :: Maybe Ticker -> String
showDecodedTicker Nothing = "No ticker here"
showDecodedTicker (Just a) = show a
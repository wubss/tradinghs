{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import GHC.Generics

import Network.Wreq

import Data.Maybe

import Control.Lens

import Data.Aeson

import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = getKrakenPair "ETHXBT"

getKrakenPair :: String -> IO ()
getKrakenPair pair = do
    requestBody <- makePublicGetRequest $ "Ticker?pair=" ++ pair
    case requestBody of
        Just a -> do
            --putStrLn $ show a
            let ticker = tickerFromJSON a
            print ticker
        Nothing -> putStrLn "Request failed."
    --print ticker
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
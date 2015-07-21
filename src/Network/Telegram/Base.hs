{-# LANGUAGE OverloadedStrings #-}

module Network.Telegram.Base where

import Network.HTTP.Conduit
import Network.URL
import Data.ByteString.Lazy
import Data.Aeson
import Data.Maybe
import Control.Monad
import Network.Telegram.ResponseTypes
import Network.Telegram.RequestTypes

baseURL :: URL
baseURL = fromJust $ importURL "https://api.telegram.org"

addParam :: (String, String) -> URL -> URL
addParam = flip add_param

addParams :: [(String, String)] -> URL -> URL
addParams ps url = Prelude.foldl (flip addParam) url ps

addToPath :: String -> URL -> URL
addToPath s url = url { url_path = url_path' ++ s }
    where url_path' = url_path url

withToken :: Token -> URL -> URL
withToken t url = url { url_path = "bot" ++ t ++ url_path'}
    where url_path' = url_path url

getMeURL :: Token -> URL
getMeURL t = withToken t $
    addToPath "/getMe" $
    baseURL

makeRequest :: FromJSON a => URL -> IO (Maybe a)
makeRequest = liftM decode . simpleHttp . exportURL

getMe :: Token -> IO (Maybe (TelegramResponse User))
getMe = makeRequest . getMeURL

getUpdatesURL :: Token -> GetUpdatesParams -> URL
getUpdatesURL t p = withToken t $
    addParams (params p) $
    addToPath "/getUpdates" $
    baseURL

getUpdates :: Token -> GetUpdatesParams
           -> IO (Maybe (TelegramResponse [Update]))
getUpdates t p = makeRequest $ getUpdatesURL t p

sendMessageURL :: Token -> SendMessageParams -> URL
sendMessageURL t p = withToken t $
    addParams (params p) $
    addToPath "/sendMessage" $
    baseURL

sendMessage :: Token -> SendMessageParams -> IO (Maybe (TelegramResponse Message))
sendMessage t p = makeRequest $ sendMessageURL t p

setWebhookURL :: Token -> String -> URL
setWebhookURL t s = withToken t $
    addParams [("url", s)] $
    addToPath "/setWebhook" $
    baseURL

setWebhook :: Token -> String -> IO ByteString
setWebhook t s = simpleHttp $ exportURL $ setWebhookURL t s

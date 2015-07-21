{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.Telegram.RequestTypes where

import Data.Default
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (toString)


type Token = String

class URLParams a where
    params :: a -> [(String, String)]

data GetUpdatesParams = GetUpdatesParams
    { getUpdatesOffset :: Maybe Int
    , getUpdatesLimit :: Maybe Int
    , getUpdatesTimeout :: Maybe Int
    } deriving Show

instance Default GetUpdatesParams where
    def = GetUpdatesParams
        { getUpdatesOffset = Nothing
        , getUpdatesLimit = Nothing
        , getUpdatesTimeout = Nothing
        }

instance URLParams GetUpdatesParams where
    params p = catMaybes mparams
        where mparams = [ createParam "offset" $ encode' <$> getUpdatesOffset p
                        , createParam "limit" $ encode' <$> getUpdatesLimit p
                        , createParam "timeout" $ encode' <$> getUpdatesTimeout p
                        ]

data SendMessageParams = SendMessageParams
    { sendMessageChatId :: Int
    , sendMessageText :: String
    , sendMessageDisableWebPagePreview :: Maybe Bool
    , sendMessageReplyToMessageId :: Maybe Int
    }

instance URLParams SendMessageParams where
    params p = catMaybes
        [ createParam "chat_id" $ Just $ encode' $ sendMessageChatId p
        , createParam "text" $ Just $ sendMessageText p
        , createParam "disable_web_page_preview" $
            encode' <$> sendMessageDisableWebPagePreview p
        , createParam "reply_to_message_id" $
            encode' <$> sendMessageReplyToMessageId p
        ]

encode' :: ToJSON a => a -> String
encode' = toString . encode

createParam :: String -> Maybe String -> Maybe (String, String)
createParam key value = (key,) <$> value

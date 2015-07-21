{-# LANGUAGE OverloadedStrings #-}

module Network.Telegram.ResponseTypes where

import Data.Aeson
import Control.Monad
import Control.Applicative


data TelegramResponse a = TelegramResponse
    { responseOk :: Bool
    , responseDescription :: Maybe String
    , responseResult :: Maybe a
    } deriving Show

instance FromJSON a => FromJSON (TelegramResponse a) where
    parseJSON (Object v) = TelegramResponse <$>
                           v .: "ok" <*>
                           v .:? "description" <*>
                           v .:? "result"
    parseJSON _          = mzero

data Update = Update
    { updateId :: Int
    , updateMessage :: Maybe Message
    } deriving (Show)

instance FromJSON Update where
    parseJSON (Object v) = Update <$>
                           v .: "update_id" <*>
                           v .:? "message"
    parseJSON _          = mzero

data User = User
    { userId :: Int
    , userFirstName :: String
    , userLastName :: Maybe String
    , userUsername :: Maybe String
    } deriving Show

instance FromJSON User where
    parseJSON (Object v) = User <$>
                           v .: "id" <*>
                           v .: "first_name" <*>
                           v .:? "last_name" <*>
                           v .:? "username"
    parseJSON _          = mzero

data GroupChat = GroupChat
    { groupchatId :: Int
    , groupchatTitle :: String
    } deriving Show

instance FromJSON GroupChat where
    parseJSON (Object v) = GroupChat <$>
                           v .: "id" <*>
                           v .: "title"
    parseJSON _          = mzero

data Chat = ChatUser User
          | ChatGroup GroupChat deriving Show

instance FromJSON Chat where
    parseJSON j =
        (ChatUser <$> parseJSON j) <|> (ChatGroup <$> parseJSON j)

data Message = Message
    { messageId :: Int
    , messageFrom :: User
    , messageDate :: Int
    , messageChat :: Chat
    , messageForwardFrom :: Maybe User
    , messageForwardDate :: Maybe Int
    , messageReplyToMessage :: Maybe Message
    , messageText :: Maybe String
    , messageAudio :: Maybe Audio
    , messageDocument :: Maybe Document
    , messagePhoto :: Maybe [PhotoSize]
    , messageSticker :: Maybe Sticker
    , messageVideo :: Maybe Video
    , messageContact :: Maybe Contact
    , messageLocation :: Maybe Location
    , messageNewChatParticipant :: Maybe User
    , messageLeftChatParticipant :: Maybe User
    , messageNewChatTitle :: Maybe String
    , messageNewChatPhoto :: Maybe [PhotoSize]
    , messageDeleteChatPhoto :: Maybe Bool
    , messageGroupChatCreated :: Maybe Bool
    } deriving Show

instance FromJSON Message where
    parseJSON (Object v) = Message <$>
                           v .: "message_id" <*>
                           v .: "from" <*>
                           v .: "date" <*>
                           v .: "chat" <*>
                           v .:? "forward_from" <*>
                           v .:? "forward_date" <*>
                           v .:? "reply_to_message" <*>
                           v .:? "text" <*>
                           v .:? "audio" <*>
                           v .:? "document" <*>
                           v .:? "photo" <*>
                           v .:? "sticker" <*>
                           v .:? "video" <*>
                           v .:? "contact" <*>
                           v .:? "location" <*>
                           v .:? "new_chat_participant" <*>
                           v .:? "left_chat_participant" <*>
                           v .:? "new_chat_title" <*>
                           v .:? "new_chat_photo" <*>
                           v .:? "delete_chat_photo" <*>
                           v .:? "group_chat_created"
    parseJSON _          = mzero

data PhotoSize = PhotoSize
    { photoSizeFileId :: String
    , photoSizeWidth :: Int
    , photoSizeHeight :: Int
    , photoSizeFileSize :: Maybe Int
    } deriving Show

instance FromJSON PhotoSize where
    parseJSON (Object v) = PhotoSize <$>
                           v .: "file_id" <*>
                           v .: "width" <*>
                           v .: "height" <*>
                           v .:? "file_size"
    parseJSON _          = mzero

data Audio = Audio
    { audioFileId :: String
    , audioDuration :: Int
    , audioMimeType :: Maybe String
    , audioFileSize :: Maybe Int
    } deriving Show

instance FromJSON Audio where
    parseJSON (Object v) = Audio <$>
                           v .: "file_id" <*>
                           v .: "duration" <*>
                           v .:? "mime_type" <*>
                           v .:? "file_size"
    parseJSON _          = mzero

data Document = Document
    { documentFileId :: String
    , documentThumb :: PhotoSize
    , documentFileName :: Maybe String
    , documentMimeType :: Maybe String
    , documentFileSize :: Maybe Int
    } deriving Show

instance FromJSON Document where
    parseJSON (Object v) = Document <$>
                           v .: "file_id" <*>
                           v .: "thumb" <*>
                           v .:? "file_name" <*>
                           v .:? "mime_type" <*>
                           v .:? "file_size"
    parseJSON _          = mzero

data Sticker = Sticker
    { stickerFileId :: String
    , stickerWidth :: Int
    , stickerHeight :: Int
    , stickerThumb :: PhotoSize
    , stickerFileSize :: Maybe Int
    } deriving Show

instance FromJSON Sticker where
    parseJSON (Object v) = Sticker <$>
                           v .: "file_id" <*>
                           v .: "width" <*>
                           v .: "height" <*>
                           v .: "thumb" <*>
                           v .:? "file_size"
    parseJSON _          = mzero

data Video = Video
    { videoFileId :: String
    , videoWidth :: Int
    , videoHeight :: Int
    , videoDuration :: Int
    , videoThumb :: PhotoSize
    , videoMimeType :: Maybe String
    , videoFileSize :: Maybe Int
    , videoCaption :: Maybe String
    } deriving Show

instance FromJSON Video where
    parseJSON (Object v) = Video <$>
                           v .: "file_id" <*>
                           v .: "width" <*>
                           v .: "height" <*>
                           v .: "duration" <*>
                           v .: "thumb" <*>
                           v .:? "mime_type" <*>
                           v .:? "file_size" <*>
                           v .:? "caption"
    parseJSON _          = mzero

data Contact = Contact
    { contactPhoneNumber :: String
    , contactFirstName :: String
    , contactLastName :: Maybe String
    , contactUserId :: Maybe String
    } deriving Show

instance FromJSON Contact where
    parseJSON (Object v) = Contact <$>
                           v .: "phone_number" <*>
                           v .: "first_name" <*>
                           v .:? "last_name" <*>
                           v .:? "user_id"
    parseJSON _          = mzero

data Location = Location
    { locationLongitude :: Float
    , locationLatitude :: Float
    } deriving Show

instance FromJSON Location where
    parseJSON (Object v) = Location <$>
                           v .: "longitude" <*>
                           v .: "latitude"
    parseJSON _          = mzero

data UserProfilePhotos = UserProfilePhotos
    { userProfilePhotosTotalCount :: Int
    , userProfilePhotosPhotos :: [[PhotoSize]]
    } deriving Show

instance FromJSON UserProfilePhotos where
    parseJSON (Object v) = UserProfilePhotos <$>
                           v .: "total_count" <*>
                           v .: "photos"
    parseJSON _          = mzero

data ReplyKeyboardMarkup = ReplyKeyboardMarkup
    { replyKeyboardMarkupKeyboard :: [[String]]
    , replyKeyboardMarkupResizeKeyboard :: Maybe Bool
    , replyKeyboardMarkupOneTimeKeyboard :: Maybe Bool
    , replyKeyboardMarkupSelective :: Maybe Bool
    } deriving Show

instance FromJSON ReplyKeyboardMarkup where
    parseJSON (Object v) = ReplyKeyboardMarkup <$>
                           v .: "keyboard" <*>
                           v .:? "resize_keyboard" <*>
                           v .:? "one_time_keyboard" <*>
                           v .:? "selective"
    parseJSON _          = mzero

data ReplyKeyboardHide = ReplyKeyboardHide
    { replyKeyboardHideHideKeyboard :: Bool
    , replyKeyboardHideSelective :: Maybe Bool
    } deriving Show

instance FromJSON ReplyKeyboardHide where
    parseJSON (Object v) = ReplyKeyboardHide <$>
                           v .: "hide_keyboard" <*>
                           v .:? "selective"
    parseJSON _          = mzero

data ForceReply = ForceReply
    { forceReplyForceReply :: Bool
    , forceReplySelective :: Maybe Bool
    } deriving Show

instance FromJSON ForceReply where
    parseJSON (Object v) = ForceReply <$>
                           v .: "force_reply" <*>
                           v .:? "selective"
    parseJSON _          = mzero

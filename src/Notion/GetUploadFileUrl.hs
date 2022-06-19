{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Notion.GetUploadFileUrl where

import           Control.Exception.Safe (MonadThrow)
import           Control.Monad.IO.Class
import           Data.Aeson             (FromJSON (..), ToJSON (..),
                                         genericParseJSON, genericToJSON)
import           Data.Aeson.Casing      (aesonDrop, camelCase)
import qualified Data.ByteString.Char8  as BC
import           Data.Text              (Text)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           Network.HTTP.Simple
import           System.FilePath.Posix  (takeFileName)

data ReqBody =
  ReqBody { _reqBucket      :: Text
          , _reqContentType :: Text
          , _reqName        :: Text
          } deriving (Eq, Show, Generic)

instance ToJSON ReqBody where
  toJSON = genericToJSON $ aesonDrop 4 camelCase

instance FromJSON ReqBody where
  parseJSON = genericParseJSON $ aesonDrop 4 camelCase

simpleRequestBody :: String -> ReqBody
simpleRequestBody filePath =
  ReqBody { _reqBucket = "secure"
          , _reqContentType = ""
          , _reqName = T.pack . takeFileName $ filePath }


data ResBody =
  ResBody { _resUrl          :: Text
          , _resSignedPutUrl :: Text
          } deriving (Eq, Show, Generic)

instance ToJSON ResBody where
  toJSON = genericToJSON $ aesonDrop 4 camelCase

instance FromJSON ResBody where
  parseJSON = genericParseJSON $ aesonDrop 4 camelCase

getS3URL :: ResBody -> String
getS3URL = T.unpack . _resUrl

getS3SignedPutURL :: ResBody -> String
getS3SignedPutURL = T.unpack . _resSignedPutUrl


endpoint :: String
endpoint = "https://www.notion.so/api/v3/getUploadFileUrl"

userAgent :: String
userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36"

getUploadFileUrl :: (MonadThrow m, MonadIO m) => String -> String -> m ResBody
getUploadFileUrl token filePath = do
  req <- parseRequest endpoint
  let req' = setRequestMethod "POST"
           . setRequestHeader "Cookie" [BC.pack $ "token_v2=" ++ token]
           . setRequestHeader "User-Agent" [BC.pack userAgent]
           . setRequestBodyJSON (simpleRequestBody filePath)
           $ req
  res <- httpJSON req'
  return $ getResponseBody res

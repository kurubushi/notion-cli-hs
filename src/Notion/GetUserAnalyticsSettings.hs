{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Notion.GetUserAnalyticsSettings where

import           Control.Exception.Safe (MonadThrow)
import           Control.Monad.IO.Class
import           Data.Aeson             (FromJSON (..), ToJSON (..),
                                         genericParseJSON, genericToJSON)
import           Data.Aeson.Casing      (aesonDrop, snakeCase)
import qualified Data.ByteString.Char8  as BC
import           GHC.Generics           (Generic)
import           Network.HTTP.Simple

type UUID = String
type URL = String
type Token = String

newtype ReqBody = ReqBody { _reqPlatform :: String
                          } deriving (Eq, Show, Generic)

instance ToJSON ReqBody where
  toJSON = genericToJSON $ aesonDrop 4 snakeCase

defaultReqBody :: ReqBody
defaultReqBody = ReqBody { _reqPlatform = "web" }

newtype ResBody = ResBody { _resUserId :: UUID
                          } deriving (Eq, Show, Generic)

instance FromJSON ResBody where
  parseJSON = genericParseJSON $ aesonDrop 4 snakeCase

endpoint :: URL
endpoint = "https://www.notion.so/api/v3/getUserAnalyticsSettings"

userAgent :: String
userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36"

getUserID :: (MonadThrow m, MonadIO m) => Token -> m UUID
getUserID token = do
  req <- parseRequest endpoint
  let req' = setRequestMethod "POST"
           . setRequestHeader "Cookie" [BC.pack $ "token_v2=" ++ token]
           . setRequestHeader "User-Agent" [BC.pack userAgent]
           . setRequestBodyJSON defaultReqBody
           $ req
  res <- httpJSON req'
  return . _resUserId . getResponseBody $ res

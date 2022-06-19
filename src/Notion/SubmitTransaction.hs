{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Notion.SubmitTransaction where

import           Control.Exception.Safe             (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Data.Aeson                         (Options (..),
                                                     SumEncoding (..),
                                                     ToJSON (..), encode,
                                                     genericToJSON)
import           Data.Aeson.Casing                  (aesonDrop, snakeCase)
import qualified Data.ByteString.Char8              as BC
import qualified Data.Text                          as T
import qualified Data.UUID                          as UUID
import qualified Data.UUID.V4                       as UUIDv4
import qualified Data.UnixTime                      as UT
import           GHC.Generics                       (Generic)
import           Network.HTTP.Simple
import           Network.Mime                       (defaultMimeLookup)
import           Notion.GetUserAnalyticsSettings    (getUserID)
import           Notion.SubmitTransaction.Operation (Arguments (..),
                                                     Operation (..), URL, UUID)
import qualified Notion.SubmitTransaction.Operation as Op

type Token = String

newtype ReqBody = ReqBody { _reqOperations :: [Operation] }
  deriving (Eq, Show, Generic)

instance ToJSON ReqBody where
  toJSON = genericToJSON $ aesonDrop 4 snakeCase

endpoint :: URL
endpoint = "https://www.notion.so/api/v3/submitTransaction"

userAgent :: String
userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36"

genUUID :: MonadIO m => m UUID
genUUID = UUID.toString <$> liftIO UUIDv4.nextRandom

getUnixTime :: MonadIO m => m Int
getUnixTime = do
  time <- liftIO UT.getUnixTime
  unixtime <- liftIO $ UT.formatUnixTime "%s" time
  return . read . BC.unpack $ unixtime

post :: (MonadIO m, MonadThrow m) => Token -> [Operation] -> m ()
post token ops = do
  let body = ReqBody { _reqOperations = ops }
  req <- parseRequest endpoint
  let req' = setRequestMethod "POST"
           . setRequestHeader "Cookie" [BC.pack $ "token_v2=" ++ token]
           . setRequestHeader "User-Agent" [BC.pack userAgent]
           . setRequestBodyJSON body
           $ req
  httpNoBody req'
  return ()

appendRecord :: (MonadIO m, MonadThrow m) => Token -> UUID -> String -> m UUID
appendRecord token collectionID recordTitle = do
  blockID <- genUUID
  userID <- getUserID token
  unixTime <- getUnixTime
  post token $ Op.appendRecord blockID userID unixTime collectionID recordTitle
  return blockID

appendS3File :: (MonadIO m, MonadThrow m) => Token -> UUID -> URL -> m UUID
appendS3File token pageID url = do
  blockID <- genUUID
  userID <- getUserID token
  unixTime <- getUnixTime
  post token $ Op.appendS3File blockID userID unixTime pageID url
  return blockID

appendText :: (MonadIO m, MonadThrow m) => Token -> UUID -> String -> m UUID
appendText token pageID content = do
  blockID <- genUUID
  userID <- getUserID token
  unixTime <- getUnixTime
  post token $ Op.appendText blockID userID unixTime pageID content
  return blockID

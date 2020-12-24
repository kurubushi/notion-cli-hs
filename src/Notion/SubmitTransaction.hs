{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Notion.SubmitTransaction where

import           Control.Exception.Safe (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (Options (..), SumEncoding (..),
                                         ToJSON (..), genericToJSON)
import           Data.Aeson.Casing      (aesonDrop, snakeCase)
import qualified Data.ByteString.Char8  as BC
import qualified Data.Text              as T
import qualified Data.UUID              as UUID
import qualified Data.UUID.V4           as UUIDv4
import           GHC.Generics           (Generic)
import           Network.HTTP.Simple
import           Network.Mime           (defaultMimeLookup)

type URL = String
type Token = String
type UUID = String

newtype ReqBody = ReqBody { _reqOperations :: [Operation] }
  deriving (Eq, Show, Generic)

instance ToJSON ReqBody where
  toJSON = genericToJSON $ aesonDrop 4 snakeCase

data Operation = Operation { _opId      :: UUID
                           , _opTable   :: String
                           , _opPath    :: [String]
                           , _opCommand :: String
                           , _opArgs    :: Arguments
                           } deriving (Eq, Show, Generic)

instance ToJSON Operation where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase

data Arguments = ArgumentsObj { _argId            :: Maybe UUID
                              , _argType          :: Maybe String
                              , _argVersion       :: Maybe Int
                              , _argParentId      :: Maybe UUID
                              , _argParentTable   :: Maybe String
                              , _argAlive         :: Maybe Bool
                              , _argSource        :: Maybe [[URL]]
                              , _argDisplaySource :: Maybe URL
                              }
               | ArgumentsList [[String]]
               deriving (Eq, Show, Generic)

instance ToJSON Arguments where
  toJSON = genericToJSON $ (aesonDrop 4 snakeCase) { omitNothingFields = True, sumEncoding = UntaggedValue }

defaultArgumentsObj :: Arguments
defaultArgumentsObj = ArgumentsObj { _argId = Nothing
                                   , _argType = Nothing
                                   , _argVersion = Nothing
                                   , _argParentId = Nothing
                                   , _argParentTable = Nothing
                                   , _argAlive = Nothing
                                   , _argSource = Nothing
                                   , _argDisplaySource = Nothing
                                   }

endpoint :: URL
endpoint = "https://www.notion.so/api/v3/submitTransaction"

s3URLPrefix :: URL
s3URLPrefix = "https://s3-us-west-2.amazonaws.com/secure.notion-static.com/"

genUUID :: MonadIO m => m UUID
genUUID = UUID.toString <$> liftIO UUIDv4.nextRandom

appendRecord :: (MonadIO m, MonadThrow m) => Token -> UUID -> String -> m UUID
appendRecord token collectionID recordTitle = do
  uuid <- genUUID
  let createOp = Operation { _opId = uuid
                           , _opTable = "block"
                           , _opPath = []
                           , _opCommand = "set"
                           , _opArgs = defaultArgumentsObj
                                       { _argId = Just uuid
                                       , _argType = Just "page"
                                       , _argVersion = Just 1
                                       , _argParentId = Just collectionID
                                       , _argParentTable = Just "collection"
                                       , _argAlive = Just True
                                       }
                           }
  let setTitleOp = Operation { _opId = uuid
                             , _opTable = "block"
                             , _opPath = ["properties", "title"]
                             , _opCommand = "set"
                             , _opArgs = ArgumentsList [[recordTitle]]
                             }
  let body = ReqBody { _reqOperations = [createOp, setTitleOp] }
  req <- parseRequest endpoint
  let req' = setRequestMethod "POST"
           . setRequestHeader "Cookie" [BC.pack $ "token_v2=" ++ token]
           . setRequestBodyJSON body
           $ req
  _ <- httpNoBody req'
  return uuid

getS3FileID :: URL -> UUID
getS3FileID = takeWhile (/= '/') . drop (length s3URLPrefix)

getBlockType :: URL -> String
getBlockType = conv . takeWhile (/= '/') . BC.unpack . defaultMimeLookup . T.pack
  where
    conv "image" = "image"
    conv "audio" = "audio"
    conv "video" = "video"
    conv _       = "file"

appendS3File :: (MonadIO m, MonadThrow m) => Token -> UUID -> URL -> m UUID
appendS3File token pageID url = do
  uuid <- genUUID
  let setOp = Operation { _opId = uuid
                        , _opTable = "block"
                        , _opPath = []
                        , _opCommand = "set"
                        , _opArgs = defaultArgumentsObj
                                    { _argId = Just uuid
                                    , _argType = Just $ getBlockType url
                                    , _argVersion = Just 1
                                    , _argParentId = Just pageID
                                    , _argParentTable = Just "block"
                                    , _argAlive = Just True
                                    }
                        }
  let listOp = Operation { _opId = pageID
                         , _opTable = "block"
                         , _opPath = ["content"]
                         , _opCommand = "listAfter"
                         , _opArgs = defaultArgumentsObj { _argId = Just uuid }
                         }
  let updatePropOp = Operation { _opId = uuid
                               , _opTable = "block"
                               , _opPath = ["properties"]
                               , _opCommand = "update"
                               , _opArgs = defaultArgumentsObj { _argSource = Just [[url]] }
                               }
  let updateFmtOp = Operation { _opId = uuid
                              , _opTable = "block"
                              , _opPath = ["format"]
                              , _opCommand = "update"
                              , _opArgs = defaultArgumentsObj { _argDisplaySource = Just url }
                              }
  let regSrcOp = Operation { _opId = uuid
                           , _opTable = "block"
                           , _opPath = ["file_ids"]
                           , _opCommand = "listAfter"
                           , _opArgs = defaultArgumentsObj { _argId = Just $ getS3FileID url }
                           }
  let body = ReqBody { _reqOperations = [ setOp
                                        , listOp
                                        , updatePropOp
                                        , updateFmtOp
                                        , regSrcOp
                                        ]
                     }
  req <- parseRequest endpoint
  let req' = setRequestMethod "POST"
           . setRequestHeader "Cookie" [BC.pack $ "token_v2=" ++ token]
           . setRequestBodyJSON body
           $ req
  _ <- httpNoBody req'
  return uuid

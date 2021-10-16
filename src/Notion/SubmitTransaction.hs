{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Notion.SubmitTransaction where

import           Control.Exception.Safe          (MonadThrow)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Data.Aeson                      (Options (..),
                                                  SumEncoding (..), ToJSON (..),
                                                  encode, genericToJSON)
import           Data.Aeson.Casing               (aesonDrop, snakeCase)
import qualified Data.ByteString.Char8           as BC
import qualified Data.Text                       as T
import qualified Data.UUID                       as UUID
import qualified Data.UUID.V4                    as UUIDv4
import qualified Data.UnixTime                   as UT
import           GHC.Generics                    (Generic)
import           Network.HTTP.Simple
import           Network.Mime                    (defaultMimeLookup)
import           Notion.GetUserAnalyticsSettings (getUserID)

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

defaultOperation :: Operation
defaultOperation = Operation { _opId = ""
                             , _opTable = "block"
                             , _opPath = []
                             , _opCommand = "set"
                             , _opArgs = defaultArgumentsObj
                             }

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
               | ArgumentString String
               | ArgumentInt Int
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

getUnixTime :: MonadIO m => m Int
getUnixTime = do
  time <- liftIO UT.getUnixTime
  unixtime <- liftIO $ UT.formatUnixTime "%s" time
  return . read . BC.unpack $ unixtime

defaultProps :: UUID -> UUID -> Int -> [Operation]
defaultProps blockID userID unixTime
  = [ createdBy
    , createdByTable
    , createdAt
    , editedBy
    , editedByTable
    , editedAt
    ]
  where
    time = unixTime * 1000
    set = defaultOperation { _opId = blockID, _opCommand = "set" }
    createdBy = set { _opPath = ["created_by_id"]
                    , _opArgs = ArgumentString userID
                    }
    createdByTable = set { _opPath = ["created_by_table"]
                         , _opArgs = ArgumentString "notion_user"
                         }
    createdAt = set { _opPath = ["created_time"]
                    , _opArgs = ArgumentInt time
                    }
    editedBy = set { _opPath = ["last_edited_by_id"]
                   , _opArgs = ArgumentString userID
                   }
    editedByTable = set { _opPath = ["last_edited_by_table"]
                        , _opArgs = ArgumentString "notion_user"
                        }
    editedAt = set { _opPath = ["last_edited_time"]
                   , _opArgs = ArgumentInt time
                   }

appendRecord :: (MonadIO m, MonadThrow m) => Token -> UUID -> String -> m UUID
appendRecord token collectionID recordTitle = do
  blockID <- genUUID
  userID <- getUserID token
  unixTime <- getUnixTime
  let set = defaultOperation { _opId = blockID, _opCommand = "set" }
  let create = set { _opArgs = defaultArgumentsObj
                               { _argId = Just blockID
                               , _argType = Just "page"
                               , _argVersion = Just 1
                               , _argParentId = Just collectionID
                               , _argParentTable = Just "collection"
                               , _argAlive = Just True
                               }
                   }
  let title = set { _opPath = ["properties", "title"]
                  , _opArgs = ArgumentsList [[recordTitle]]
                  }
  let body = ReqBody { _reqOperations = [ create
                                        , title
                                        ]
                                        ++ defaultProps blockID userID unixTime
                     }
  req <- parseRequest endpoint
  let req' = setRequestMethod "POST"
           . setRequestHeader "Cookie" [BC.pack $ "token_v2=" ++ token]
           . setRequestBodyJSON body
           $ req
  _ <- httpNoBody req'
  return blockID

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
  blockID <- genUUID
  userID <- getUserID token
  unixTime <- getUnixTime
  let setOp = defaultOperation
              { _opId = blockID
              , _opCommand = "set"
              , _opArgs = defaultArgumentsObj
                          { _argId = Just blockID
                          , _argType = Just $ getBlockType url
                          , _argVersion = Just 1
                          , _argParentId = Just pageID
                          , _argParentTable = Just "block"
                          , _argAlive = Just True
                          }
              }
  let listOp = defaultOperation
               { _opId = pageID
               , _opCommand = "listAfter"
               , _opPath = ["content"]
               , _opArgs = defaultArgumentsObj { _argId = Just blockID }
               }
  let updatePropOp = defaultOperation
                     { _opId = blockID
                     , _opPath = ["properties"]
                     , _opCommand = "update"
                     , _opArgs = defaultArgumentsObj { _argSource = Just [[url]] }
                     }
  let updateFmtOp = defaultOperation
                    { _opId = blockID
                    , _opPath = ["format"]
                    , _opCommand = "update"
                    , _opArgs = defaultArgumentsObj { _argDisplaySource = Just url }
                    }
  let regSrcOp = defaultOperation
                 { _opId = blockID
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
                                        ++ defaultProps blockID userID unixTime
                     }
  req <- parseRequest endpoint
  let req' = setRequestMethod "POST"
           . setRequestHeader "Cookie" [BC.pack $ "token_v2=" ++ token]
           . setRequestBodyJSON body
           $ req
  _ <- httpNoBody req'
  return blockID

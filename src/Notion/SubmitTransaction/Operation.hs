{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Notion.SubmitTransaction.Operation
  (-- types
    URL
  , UUID
  , Operation(..)
  , Arguments(..)
   -- operations
  , defaultOperation
  , defaultArgumentsObj
  , updateWithdefaultProps
  , appendRecord
  , appendS3File
  , appendText
  ) where

import           Data.Aeson            (Options (..), SumEncoding (..),
                                        ToJSON (..))
import qualified Data.Aeson            as A
import qualified Data.Aeson.Casing     as AC
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import           GHC.Generics          (Generic)
import qualified Network.Mime          as Mime

type URL = String
type UUID = String

data Operation = Operation
  { _opId      :: UUID
  , _opTable   :: String
  , _opPath    :: [String]
  , _opCommand :: String
  , _opArgs    :: Arguments
  } deriving (Eq, Show, Generic)

instance ToJSON Operation where
  toJSON = A.genericToJSON $ AC.aesonDrop (length prefix) AC.snakeCase
    where
      prefix :: String
      prefix = "_op"

data Arguments
  = ArgumentsObj
      { _argId            :: Maybe UUID
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
  toJSON = A.genericToJSON
             $ (AC.aesonDrop (length prefix) AC.snakeCase)
                 { omitNothingFields = True
                 , sumEncoding = UntaggedValue
                 }
    where
      prefix :: String
      prefix = "_arg"


defaultOperation :: Operation
defaultOperation = Operation
  { _opId = ""
  , _opTable = "block"
  , _opPath = []
  , _opCommand = "set"
  , _opArgs = defaultArgumentsObj
  }

defaultArgumentsObj :: Arguments
defaultArgumentsObj = ArgumentsObj
  { _argId = Nothing
  , _argType = Nothing
  , _argVersion = Nothing
  , _argParentId = Nothing
  , _argParentTable = Nothing
  , _argAlive = Nothing
  , _argSource = Nothing
  , _argDisplaySource = Nothing
  }

updateWithdefaultProps :: UUID -> UUID -> Int -> [Operation]
updateWithdefaultProps blockID userID unixTime
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
    createdBy
      = set { _opPath = ["created_by_id"]
            , _opArgs = ArgumentString userID
            }
    createdByTable
      = set { _opPath = ["created_by_table"]
            , _opArgs = ArgumentString "notion_user"
            }
    createdAt
      = set { _opPath = ["created_time"]
            , _opArgs = ArgumentInt time
            }
    editedBy
      = set { _opPath = ["last_edited_by_id"]
            , _opArgs = ArgumentString userID
            }
    editedByTable
      = set { _opPath = ["last_edited_by_table"]
            , _opArgs = ArgumentString "notion_user"
            }
    editedAt
      = set { _opPath = ["last_edited_time"]
            , _opArgs = ArgumentInt time
            }

appendRecord :: UUID -> UUID -> Int -> UUID -> String -> [Operation]
appendRecord blockID userID unixTime collectionID recordTitle
  = [ createBlock
    , setTitle
    ] ++ updateWithdefaultProps blockID userID unixTime
  where
    createBlock
      = defaultOperation
          { _opId = blockID
          , _opCommand = "set"
          , _opArgs
              = defaultArgumentsObj
                  { _argId = Just blockID
                  , _argType = Just "page"
                  , _argVersion = Just 1
                  , _argParentId = Just collectionID
                  , _argParentTable = Just "collection"
                  , _argAlive = Just True
                  }
          }
    setTitle
      = defaultOperation
          { _opId = blockID
          , _opCommand = "set"
          , _opPath = ["properties", "title"]
          , _opArgs = ArgumentsList [[recordTitle]]
          }

appendS3File :: UUID -> UUID -> Int -> UUID -> URL -> [Operation]
appendS3File blockID userID unixTime pageID url
  = [ createBlock
    , appendToPage
    , updateProp
    , updateFmt
    , registerS3File
    ] ++ updateWithdefaultProps blockID userID unixTime
  where
    createBlock
      = defaultOperation
          { _opId = blockID
          , _opCommand = "set"
          , _opArgs
              = defaultArgumentsObj
                  { _argId = Just blockID
                  , _argType = Just $ getBlockType url
                  , _argVersion = Just 1
                  , _argParentId = Just pageID
                  , _argParentTable = Just "block"
                  , _argAlive = Just True
                  }
          }
    appendToPage
      = defaultOperation
          { _opId = pageID
          , _opCommand = "listAfter"
          , _opPath = ["content"]
          , _opArgs = defaultArgumentsObj { _argId = Just blockID }
          }
    updateProp
      = defaultOperation
          { _opId = blockID
          , _opPath = ["properties"]
          , _opCommand = "update"
          , _opArgs = defaultArgumentsObj { _argSource = Just [[url]] }
          }
    updateFmt
      = defaultOperation
          { _opId = blockID
          , _opPath = ["format"]
          , _opCommand = "update"
          , _opArgs = defaultArgumentsObj { _argDisplaySource = Just url }
          }
    registerS3File
      = defaultOperation
          { _opId = blockID
          , _opPath = ["file_ids"]
          , _opCommand = "listAfter"
          , _opArgs = defaultArgumentsObj { _argId = Just $ getS3FileID url }
          }

getS3FileID :: URL -> UUID
getS3FileID = takeWhile (/= '/') . drop (length s3URLPrefix)
  where
    s3URLPrefix :: URL
    s3URLPrefix = "https://s3-us-west-2.amazonaws.com/secure.notion-static.com/"

getBlockType :: URL -> String
getBlockType = conv . takeWhile (/= '/') . BC.unpack . Mime.defaultMimeLookup . T.pack
  where
    conv "image" = "image"
    conv "audio" = "audio"
    conv "video" = "video"
    conv _       = "file"

appendText :: UUID -> UUID -> Int -> UUID -> String -> [Operation]
appendText blockID userID unixTime pageID content
  = [ createBlock
    , appendToPage
    , updateContent
    ] ++ updateWithdefaultProps blockID userID unixTime
  where
    createBlock
      = defaultOperation
          { _opId = blockID
          , _opCommand = "set"
          , _opArgs
              = defaultArgumentsObj
                  { _argId = Just blockID
                  , _argType = Just "text"
                  , _argVersion = Just 1
                  , _argParentId = Just pageID
                  , _argParentTable = Just "block"
                  , _argAlive = Just True
                  }
          }
    appendToPage
      = defaultOperation
          { _opId = pageID
          , _opCommand = "listAfter"
          , _opPath = ["content"]
          , _opArgs = defaultArgumentsObj { _argId = Just blockID }
          }
    updateContent
      = defaultOperation
          { _opId = blockID
          , _opCommand = "set"
          , _opPath = ["properties", "title"]
          , _opArgs = ArgumentsList [[content]]
          }

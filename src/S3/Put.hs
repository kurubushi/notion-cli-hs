{-# LANGUAGE OverloadedStrings #-}

module S3.Put where

import           Control.Exception.Safe      (MonadThrow)
import           Control.Monad.IO.Class
import qualified Data.Text                   as T
import           Network.HTTP.Client.Conduit (ManagerSettings (..),
                                              defaultManagerSettings,
                                              newManagerSettings,
                                              responseTimeoutNone)
import           Network.HTTP.Simple
import           Network.Mime                (defaultMimeLookup)

putFile :: (MonadIO m, MonadThrow m) => String -> String -> m ()
putFile url filePath = do
  manager <- newManagerSettings
           $ defaultManagerSettings { managerResponseTimeout = responseTimeoutNone }

  req <- parseRequest url
  let req' = setRequestMethod "PUT"
           . setRequestHeader "Content-Type" [defaultMimeLookup $ T.pack filePath]
           . setRequestBodyFile filePath
           . setRequestManager manager
           $ req
  res <- httpNoBody req'
  return $ getResponseBody res

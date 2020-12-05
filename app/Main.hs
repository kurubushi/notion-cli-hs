{-# LANGUAGE RecordWildCards #-}

module Main where

import           Notion.GetUploadFileUrl (getS3SignedPutURL, getS3URL,
                                          getUploadFileUrl)
import           Options.Applicative
import           S3.Put                  (putFile)
import           System.Environment      (getEnv)


data Environment = Environment { tokenV2 :: String }
  deriving (Show, Eq)

getEnvironment :: IO Environment
getEnvironment = Environment <$> getEnv "NOTION_TOKEN_V2"


data Options = S3UploadOpts { s3UploadFilePath :: String }
  deriving (Show, Eq)

s3UploadOptions :: Parser Options
s3UploadOptions = S3UploadOpts
                  <$> argument str (metavar "FILE")

options :: Parser Options
options = subparser
          $ command "s3upload" (info s3UploadOptions (progDesc "Upload a file to S3"))

withInfo :: Parser a -> ParserInfo a
withInfo opts = info (helper <*> opts) desc
  where
    desc = fullDesc
           <> header "notion-cli - Notion CLI"


exec :: Environment -> Options -> IO ()
exec env (S3UploadOpts {..}) = do
  s3URLs <- getUploadFileUrl (tokenV2 env) s3UploadFilePath
  _ <- putFile (getS3SignedPutURL s3URLs) s3UploadFilePath

  putStrLn $ "File: " ++ s3UploadFilePath
  putStrLn $ "URL: " ++ show (getS3URL s3URLs)

main :: IO ()
main = do
  env <- getEnvironment
  opts <- execParser (withInfo options)
  exec env opts

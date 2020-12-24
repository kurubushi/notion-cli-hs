{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad            (forM_)
import           Data.ConfigFile          (ConfigParser (..), emptyCP, get,
                                           readfile)
import           Data.Maybe               (fromMaybe)
import           Notion.GetUploadFileUrl  (getS3SignedPutURL, getS3URL,
                                           getUploadFileUrl)
import           Notion.SubmitTransaction (appendRecord, appendS3File)
import           Options.Applicative
import           S3.Put                   (putFile)
import           System.Directory         (getHomeDirectory)
import           System.Exit              (die)
import           System.FilePath.Posix    (takeFileName)

type UUID = String

newtype Environment = Environment { homeDir :: FilePath }
  deriving (Show, Eq)

getEnvironment :: IO Environment
getEnvironment = do
  homeDir <- getHomeDirectory
  return Environment{..}


newtype Config = Config { tokenV2 :: String }
  deriving (Show, Eq, Read)

defaultConfigFile :: FilePath -> FilePath
defaultConfigFile home = home ++ "/.notion-cli.conf"

getConfig :: FilePath -> IO Config
getConfig filePath = do
  let handle e = die $ "invalid configration file\n" ++ show e
  val <- readfile emptyCP{optionxform = id} filePath
  either handle return $ do
    cp <- val
    tokenV2 <- get cp "Cookie" "token_v2"
    return Config{..}


data Options = S3UploadOpts { s3UploadConfigFilePath :: Maybe FilePath
                            , s3UploadFilePath       :: FilePath }
             | UploadOpts { uploadDatabaseID     :: UUID
                          , uploadRecordTitle    :: Maybe String
                          , uploadConfigFilePath :: Maybe FilePath
                          , uploadFilePathes     :: [FilePath]
                          }
  deriving (Show, Eq)

s3UploadOptions :: Parser Options
s3UploadOptions = S3UploadOpts
                  <$> (optional . strOption) (long "config-file" <> metavar "FILE" <> help "Set an alternative config file")
                  <*> argument str (metavar "FILE" <> help "Select a file to upload")

uploadOptions :: Parser Options
uploadOptions = UploadOpts
                  <$> strOption (long "database-uuid" <> metavar "UUID" <> help "Set the UUID of a database")
                  <*> (optional . strOption) (long "record-title" <> metavar "TITLE" <> help "Set the Title of a created new record")
                  <*> (optional . strOption) (long "config-file" <> metavar "FILE" <> help "Set an alternative config file")
                  <*> (some . argument str) (metavar "FILES" <> help "Select files to upload")

options :: Parser Options
options = subparser
            (  command "s3upload" (withInfo s3UploadOptions "s3upload" "Upload a file to S3")
            <> command "upload" (withInfo uploadOptions "upload" "Upload a file to a database")
            )

withInfo :: Parser a -> String -> String -> ParserInfo a
withInfo opts name desc = info
                          (helper <*> opts)
                          (fullDesc <> header desc' <> progDesc desc)
  where
    desc' = "notion-cli " ++ name' ++ "- " ++ desc
    name' = if name == "" then name ++ " " else ""


exec :: Environment -> Options -> IO ()
exec env S3UploadOpts {..} = do
  conf <- getConfig $ fromMaybe (defaultConfigFile . homeDir $ env) s3UploadConfigFilePath
  s3URLs <- getUploadFileUrl (tokenV2 conf) s3UploadFilePath
  _ <- putFile (getS3SignedPutURL s3URLs) s3UploadFilePath

  putStrLn $ "File: " ++ s3UploadFilePath
  putStrLn $ "URL: " ++ show (getS3URL s3URLs)

exec env UploadOpts {..} = do
  conf <- getConfig $ fromMaybe (defaultConfigFile . homeDir $ env) uploadConfigFilePath
  let token = tokenV2 conf

  let title = fromMaybe (takeFileName . head  $ uploadFilePathes) uploadRecordTitle
  pageID <- appendRecord token uploadDatabaseID title

  forM_ uploadFilePathes $ \filePath -> do
    s3URLs <- getUploadFileUrl token filePath
    let signedPutURL = getS3SignedPutURL s3URLs
    let url = getS3URL s3URLs
    _ <- putFile signedPutURL filePath
    _ <- appendS3File token pageID url
    putStrLn $ "File: " ++ filePath
    putStrLn $ "S3URL: " ++ show url

main :: IO ()
main = do
  env <- getEnvironment
  opts <- execParser (withInfo options "" "Notion CLI")
  exec env opts

{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad            (forM_)
import           Data.Char                (isHexDigit)
import           Data.ConfigFile          (ConfigParser (..), emptyCP, get,
                                           readfile)
import           Data.Maybe               (fromMaybe)
import           Notion.GetUploadFileUrl  (getS3SignedPutURL, getS3URL,
                                           getUploadFileUrl)
import           Notion.SubmitTransaction (appendRecord, appendS3File,
                                           appendText)
import           Options.Applicative
import           S3.Put                   (putFile)
import           System.Directory         (getHomeDirectory)
import           System.Exit              (die)
import           System.FilePath.Posix    (takeFileName)

type UUID = String
type URL = String

getUUID :: URL -> Maybe UUID
getUUID = format . takeLastHex . dropID
  where
    dropID = takeWhile (/= '#')
    takeLastHex = reverse . takeWhile isHexDigit . reverse
    format st = if length st == 32
                then Just $ slice  0  8 st ++ "-"
                         ++ slice  8 12 st ++ "-"
                         ++ slice 12 16 st ++ "-"
                         ++ slice 16 20 st ++ "-"
                         ++ slice 20 32 st
                else Nothing
    slice n m = take (m - n) . drop n

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


data ParentUUID = DBUUID UUID
                | PageUUID UUID
                | PageURL URL
                deriving (Show, Eq)

data Options = S3UploadOpts { s3UploadConfigFilePath :: Maybe FilePath
                            , s3UploadFilePath       :: FilePath }
             | UploadOpts { uploadUUID           :: ParentUUID
                          , uploadRecordTitle    :: Maybe String
                          , uploadConfigFilePath :: Maybe FilePath
                          , uploadFilePathes     :: [FilePath]
                          }
             | AppendTextOpts { appendTextUUID           :: ParentUUID
                              , appendTextRecordTitle    :: Maybe String
                              , appendTextConfigFilePath :: Maybe FilePath
                              , appendTextContent        :: String
                              }
  deriving (Show, Eq)

s3UploadOptions :: Parser Options
s3UploadOptions = S3UploadOpts
                  <$> (optional . strOption) (long "config-file" <> metavar "FILE" <> help "Set an alternative config file")
                  <*> argument str (metavar "FILE" <> help "Select a file to upload")

parentUUID :: Parser ParentUUID
parentUUID = dbUUID <|> pageUUID <|> pageURL
  where
    dbUUID = DBUUID <$> strOption (long "database-uuid" <> metavar "UUID" <> help "Set the UUID of a database")
    pageUUID = PageUUID <$> strOption (long "page-uuid" <> metavar "UUID" <> help "Set the UUID of a page")
    pageURL = PageURL <$> strOption (long "page-url" <> metavar "URL" <> help "Set the URL of a page")

uploadOptions :: Parser Options
uploadOptions = UploadOpts
                <$> parentUUID
                <*> (optional . strOption) (long "record-title" <> metavar "TITLE" <> help "Set the Title of a created new record")
                <*> (optional . strOption) (long "config-file" <> metavar "FILE" <> help "Set an alternative config file")
                <*> (some . argument str) (metavar "FILES" <> help "Select files to upload")

appendTextOptions :: Parser Options
appendTextOptions = AppendTextOpts
                    <$> parentUUID
                    <*> (optional . strOption) (long "record-title" <> metavar "TITLE" <> help "Set the Title of a created new record")
                    <*> (optional . strOption) (long "config-file" <> metavar "FILE" <> help "Set an alternative config file")
                    <*> argument str (metavar "TEXT" <> help "Text to upload")

options :: Parser Options
options = subparser
            (  command "s3upload" (withInfo s3UploadOptions "s3upload" "Upload a file to S3")
            <> command "upload" (withInfo uploadOptions "upload" "Upload a file to a database")
            <> command "append-text" (withInfo appendTextOptions "append-text" "Append a text to a page")
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

  parentUUID <- case uploadUUID of
                  DBUUID uuid -> do
                    let title = fromMaybe (takeFileName . head  $ uploadFilePathes) uploadRecordTitle
                    appendRecord token uuid title
                  PageUUID uuid -> return uuid
                  PageURL url -> maybe (die "the page URL is invalid") return (getUUID url)

  forM_ uploadFilePathes $ \filePath -> do
    s3URLs <- getUploadFileUrl token filePath
    let signedPutURL = getS3SignedPutURL s3URLs
    let url = getS3URL s3URLs
    _ <- putFile signedPutURL filePath
    _ <- appendS3File token parentUUID url
    putStrLn $ "File: " ++ filePath
    putStrLn $ "S3URL: " ++ show url

exec env AppendTextOpts {..} = do
  conf <- getConfig $ fromMaybe (defaultConfigFile . homeDir $ env) appendTextConfigFilePath
  let token = tokenV2 conf

  parentUUID <- case appendTextUUID of
                  DBUUID uuid -> do
                    let title = fromMaybe "" appendTextRecordTitle
                    appendRecord token uuid title
                  PageUUID uuid -> return uuid
                  PageURL url -> maybe (die "the page URL is invalid") return (getUUID url)

  uuid <- appendText token parentUUID appendTextContent
  putStrLn $ "UUID: " ++ uuid
  return ()

main :: IO ()
main = do
  env <- getEnvironment
  opts <- execParser (withInfo options "" "Notion CLI")
  exec env opts

{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.ConfigFile         (ConfigParser (..), emptyCP, get,
                                          readfile)
import           Data.Maybe              (fromMaybe)
import           Notion.GetUploadFileUrl (getS3SignedPutURL, getS3URL,
                                          getUploadFileUrl)
import           Options.Applicative
import           S3.Put                  (putFile)
import           System.Directory        (getHomeDirectory)
import           System.Exit             (die)


data Environment = Environment { homeDir :: FilePath }
  deriving (Show, Eq)

getEnvironment :: IO Environment
getEnvironment = do
  homeDir <- getHomeDirectory
  return Environment{..}


data Config = Config { tokenV2 :: String }
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
  deriving (Show, Eq)

s3UploadOptions :: Parser Options
s3UploadOptions = S3UploadOpts
                  <$> (optional $ strOption (long "config-file" <> metavar "FILE" <> help "Set an alternative config file"))
                  <*> (argument str (metavar "FILE"))

options :: Parser Options
options = subparser
          $ command "s3upload" (withInfo s3UploadOptions "s3upload - Upload a file to S3")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) (header $ "notion-cli " ++ desc)


exec :: Environment -> Options -> IO ()
exec env (S3UploadOpts {..}) = do
  conf <- getConfig $ fromMaybe (defaultConfigFile . homeDir $ env) s3UploadConfigFilePath
  s3URLs <- getUploadFileUrl (tokenV2 conf) s3UploadFilePath
  _ <- putFile (getS3SignedPutURL s3URLs) s3UploadFilePath

  putStrLn $ "File: " ++ s3UploadFilePath
  putStrLn $ "URL: " ++ show (getS3URL s3URLs)

main :: IO ()
main = do
  env <- getEnvironment
  opts <- execParser (withInfo options "- Notion CLI")
  exec env opts

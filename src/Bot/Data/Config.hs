{-# LANGUAGE OverloadedStrings #-}
module Bot.Data.Config (getConfig) where

import qualified Data.ByteString as BS
import System.Directory (doesFileExist)
import Bot.Types (Config)
import Control.Exception (catch)
import Data.Yaml
import System.IO.Error


fallbackConfig :: FilePath
fallbackConfig = "config.yaml"

-- Falls back to a config.yaml file when it can't read the file passed
getConfig :: FilePath -> IO Config
getConfig filePath = do
  configFile <- catch (BS.readFile filePath)
                      useDefaultConfigFile

  maybeConfig <- catch (return $ decode configFile)
                       handleParseException

  case decode configFile of
    Just c -> return c
    _ -> ioError $ userError $ "Could not generate Config value from config file."

  where useDefaultConfigFile :: IOError -> IO BS.ByteString
        useDefaultConfigFile ex = do
          fileExista <- doesFileExist filePath
          if fileExista
             then ioError $ userError $ "Error reading config file: "
                                     ++ filePath
                                     ++ "Additional info: "
                                     ++ (show ex)
             else do
               putStrLn $ "Using config file: " ++ fallbackConfig
               fileExistb <- doesFileExist fallbackConfig
               if fileExistb
                  then BS.readFile fallbackConfig
                  else ioError $ userError $ "Could not find valid config file."

        -- throw an IO Error as a result of a parseexception
        handleParseException :: ParseException -> IO (Maybe Config)
        handleParseException ex  = ioError $ userError $ "Error parsing config file"
                                                      ++ filePath
                                                      ++ " or "
                                                      ++ fallbackConfig
                                                      ++ "Additional info: "
                                                      ++  (show ex)

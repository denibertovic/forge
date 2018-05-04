{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab where


import           Control.Monad    (when)
import           Data.Aeson       (FromJSON, Value (..), parseJSON, (.:))
import           Data.Yaml        (decodeFileEither)
import qualified Data.Yaml        as Y
import           System.Directory (doesFileExist, makeAbsolute)
import           System.Exit      (die)

newtype AccessToken = AccessToken String deriving (Eq, Show)

data GitlabConfig = GitlabConfig { accessToken :: AccessToken } deriving (Eq, Show)

instance FromJSON GitlabConfig where
  parseJSON (Object o) = do
    accessToken <- o .: "accessToken"
    return $ GitlabConfig $ AccessToken accessToken
  parseJSON _ = fail "Expected Object for Config value"

decodeConfig :: FilePath -> IO (Either Y.ParseException GitlabConfig)
decodeConfig p = Y.decodeFileEither p

readConfig :: FilePath -> IO (GitlabConfig)
readConfig p = do
  exists <- doesFileExist p
  when (not exists) (die "Config file does not exist.")
  c <- decodeConfig p
  case c of
    Left err -> die (show err)
    Right c  -> return c


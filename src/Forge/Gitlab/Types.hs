{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab.Types where


import           Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import qualified Data.Yaml  as Y

newtype Environment = Environment String deriving (Eq, Show)
newtype VarKey = VarKey String deriving (Eq, Show)
newtype VarValue = VarValue String deriving (Eq, Show)
newtype Url = Url String deriving (Eq, Show)
newtype AccessToken = AccessToken String deriving (Eq, Show)

data GitlabConfig = GitlabConfig { accessToken :: AccessToken } deriving (Eq, Show)

instance FromJSON GitlabConfig where
  parseJSON (Object o) = do
    accessToken <- o .: "accessToken"
    return $ GitlabConfig $ AccessToken accessToken
  parseJSON _ = fail "Expected Object for Config value"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab.Types where


import           Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import qualified Data.Aeson as JSON
import           Data.Text  as T
import qualified Data.Yaml  as Y

newtype Group = Group String deriving (Eq, Show)
newtype Project = Project String deriving (Eq, Show)
newtype Environment = Environment String deriving (Eq, Show)
newtype VarKey = VarKey String deriving (Eq, Show)
newtype VarValue = VarValue String deriving (Eq, Show)
newtype Url = Url String deriving (Eq, Show)
newtype AccessToken = AccessToken String deriving (Eq, Show)

data GitlabConfig = GitlabConfig { accessToken :: AccessToken } deriving (Eq, Show)

instance FromJSON Url where
  parseJSON (JSON.String s) = return $ Url $ T.unpack s
  parseJSON _               = fail "Expected String for Url"

instance FromJSON GitlabConfig where
  parseJSON (Object o) = do
    accessToken <- o .: "accessToken"
    return $ GitlabConfig $ AccessToken accessToken
  parseJSON _ = fail "Expected Object for Config value"


data ProjectDetails = ProjectDetails { projectId   :: Int
                                     , projectName :: String
                                     , projectUrl  :: Url
                                     } deriving (Eq, Show)

instance FromJSON ProjectDetails where
  parseJSON (Object o) = do
    projectId <- o .: "id"
    projectName <- o .: "path_with_namespace"
    projectUrl <- o .: "web_url"
    return $ ProjectDetails {..}
  parseJSON _ = fail "Expected Object for ProjectDetails value"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Github.Types where


import           Data.Aeson  (FromJSON, ToJSON, Value (..), object, parseJSON,
                              toJSON, (.:), (.:?), (.=))
import qualified Data.Aeson  as JSON
import           Data.Monoid ((<>))
import           Data.Text   as T
import qualified Data.Yaml   as Y

import           Forge.Types (AccessToken (..), Url (..))

data GithubConfig = GithubConfig { githubAccessToken :: AccessToken} deriving (Eq, Show)

instance FromJSON GithubConfig where
  parseJSON (Object o) = do
    t <- o .: "access_token"
    return $ GithubConfig $ AccessToken t
  parseJSON _ = fail "Expected Object for Config value"


data GithubIssueDetails = GithubIssueDetails { githubIssueId :: Int
                                 , githubIssueGroup          :: String
                                 , githubIssueProject        :: String
                                 , githubIssueTitle          :: String
                                 , githubIssueDescription    :: Maybe String
                                 , githubIssueUrl            :: Url
                                 } deriving (Eq, Show)

instance FromJSON GithubIssueDetails where
  parseJSON (Object o) = do
    issueId <- o .: "id"
    issueTitle <- o .: "title"
    issueDescription <- o .:? "description"
    (Url url) <- o .: "html_url"
    let splits = T.splitOn "/" $ T.pack url
    let groupName = T.unpack $ splits !! 3
    let projectName = T.unpack $ splits !! 4
    return $ GithubIssueDetails issueId groupName projectName issueTitle issueDescription (Url url)
  parseJSON _ = fail "Expected Object for GithubIssueDetails value"

instance ToJSON GithubIssueDetails where
  toJSON (GithubIssueDetails i g p t d u) = object [ "id" .= i
                                       , "group_name" .= g
                                       , "project_name" .= p
                                       , "title" .= t
                                       , "description" .= d
                                       , "html_url" .= toJSON u
                                       ]

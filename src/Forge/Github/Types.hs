{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Github.Types where

import           RIO

import           Data.Aeson  (FromJSON, ToJSON, Value (..), object, parseJSON,
                              toJSON, (.:), (.:?), (.=))
import           Data.List   ((!!))
import           Data.Text   as T

import           Forge.Types (AccessToken (..), Url (..))

data GithubConfig = GithubConfig { githubAccessToken :: AccessToken, githubApiUrl :: Url} deriving (Eq, Show)

instance FromJSON GithubConfig where
  parseJSON (Object o) = do
    t <- o .: "access_token"
    u <- o .: "api_url"
    return $ GithubConfig t u
  parseJSON _ = fail "Expected Object for Config value"


data GithubIssueDetails = GithubIssueDetails { githubIssueId :: Int
                                 , githubIssueGroup          :: T.Text
                                 , githubIssueProject        :: T.Text
                                 , githubIssueTitle          :: T.Text
                                 , githubIssueDescription    :: Maybe T.Text
                                 , githubIssueUrl            :: Url
                                 } deriving (Eq, Show)

instance FromJSON GithubIssueDetails where
  parseJSON (Object o) = do
    issueId <- o .: "id"
    issueTitle <- o .: "title"
    issueDescription <- o .:? "description"
    (Url url) <- o .: "html_url"
    let splits = T.splitOn "/" url
    let groupName = splits !! 3
    let projectName = splits !! 4
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

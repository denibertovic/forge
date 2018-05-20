{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab.Types where


import           Data.Aeson  (FromJSON, ToJSON, Value (..), object, parseJSON,
                              toJSON, (.:), (.=))
import qualified Data.Aeson  as JSON
import           Data.Monoid ((<>))
import           Data.Text   as T
import qualified Data.Yaml   as Y

import           Forge.Types (AccessToken (..), Url (..))

newtype Group = Group String deriving (Eq, Show)
newtype Project = Project String deriving (Eq, Show)
newtype Environment = Environment String deriving (Eq, Show)
newtype VarKey = VarKey String deriving (Eq, Show)
newtype VarValue = VarValue String deriving (Eq, Show)

data GitlabConfig = GitlabConfig { gitlabAccessToken :: AccessToken } deriving (Eq, Show)

instance FromJSON GitlabConfig where
  parseJSON (Object o) = do
    accessToken <- o .: "access_token"
    return $ GitlabConfig $ AccessToken accessToken
  parseJSON _ = fail "Expected Object for Config value"


data GitlabProjectDetails = GitlabProjectDetails { gitlabProjectId :: Int
                                     , gitlabProjectName           :: String
                                     , gitlabProjectUrl            :: Url
                                     } deriving (Eq, Show)

instance FromJSON GitlabProjectDetails where
  parseJSON (Object o) = do
    gitlabProjectId <- o .: "id"
    gitlabProjectName <- o .: "path_with_namespace"
    gitlabProjectUrl <- o .: "web_url"
    return $ GitlabProjectDetails {..}
  parseJSON _ = fail "Expected Object for ProjectDetails value"

instance ToJSON GitlabProjectDetails where
  toJSON (GitlabProjectDetails i n u) = object [ "id" .= i
                                         , "path_with_namespace" .= n
                                         , "web_url" .= toJSON u
                                         ]

data GitlabIssueDetails = GitlabIssueDetails { gitlabIssueId :: Int
                                 , gitlabIsueGroup           :: String
                                 , gitlabIssueProject        :: String
                                 , gitlabIssueTitle          :: String
                                 , gitlabIssueDescription    :: String
                                 , gitlabIssueUrl            :: Url
                                 } deriving (Eq, Show)

instance FromJSON GitlabIssueDetails where
  parseJSON (Object o) = do
    issueId <- o .: "id"
    issueTitle <- o .: "title"
    issueDescription <- o .: "description"
    (Url url) <- o .: "web_url"
    let splits = T.splitOn "/" $ T.pack url
    let groupName = T.unpack $ splits !! 3
    let projectName = T.unpack $ splits !! 4
    return $ GitlabIssueDetails issueId groupName projectName issueTitle issueDescription (Url url)
  parseJSON _ = fail "Expected Object for GitlabIssueDetails value"

instance ToJSON GitlabIssueDetails where
  toJSON (GitlabIssueDetails i g p t d u) = object [ "id" .= i
                                       , "group_name" .= g
                                       , "project_name" .= p
                                       , "title" .= t
                                       , "description" .= d
                                       , "web_url" .= toJSON u
                                       ]


data GitlabTodoType = GitlabTodoIssue | GitlabTodoMergeRequest deriving (Eq, Show)

instance FromJSON GitlabTodoType where
  parseJSON (JSON.String "Issue")        = return GitlabTodoIssue
  parseJSON (JSON.String "MergeRequest") = return GitlabTodoMergeRequest
  parseJSON (JSON.String s)              = fail $ "Unkown Todo Type: " <> (T.unpack s)

instance ToJSON GitlabTodoType where
  toJSON GitlabTodoIssue        = "Issue"
  toJSON GitlabTodoMergeRequest = "MergeRequest"

data GitlabTodoDetails = GitlabTodoDetails { gitlabTodoId :: Int
                               , gitlabTodoType           :: GitlabTodoType
                               , gitlabTodoGroup          :: String
                               , gitlabTodoProject        :: String
                               , gitlabTodoTitle          :: String
                               , gitlabTodoDescription    :: String
                               , gitlabTodoUrl            :: Url
                               } deriving (Eq, Show)

instance FromJSON GitlabTodoDetails where
  parseJSON (Object o) = do
    todoId <- o .: "id"
    todoType <- o .: "target_type"
    todoTitle <- (o .: "target") >>= (.: "title")
    todoDescription <- (o .: "target") >>= (.: "description")
    (Url url) <- (o .: "target") >>= (.: "web_url")
    let splits = T.splitOn "/" $ T.pack url
    let groupName = T.unpack $ splits !! 3
    let projectName = T.unpack $ splits !! 4
    return $ GitlabTodoDetails todoId todoType groupName projectName todoTitle todoDescription (Url url)
  parseJSON _ = fail "Expected Object for GitlabTodoDetails value"

instance ToJSON GitlabTodoDetails where
  toJSON (GitlabTodoDetails i ty g p t d u) = object [ "id" .= i
                                       , "group_name" .= g
                                       , "project_name" .= p
                                       , "title" .= t
                                       , "description" .= d
                                       , "type" .= toJSON ty
                                       , "web_url" .= toJSON u
                                       ]

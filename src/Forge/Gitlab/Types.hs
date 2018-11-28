{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab.Types where

import           RIO

import           Data.Aeson  (FromJSON, ToJSON, Value (..), object, parseJSON,
                              toJSON, (.:), (.=))
import qualified Data.Aeson  as JSON
import           Data.List   ((!!))
import qualified Data.Text   as T

import           Forge.Types (AccessToken (..), Url (..))

newtype Group = Group T.Text deriving (Eq, Show)
newtype Project = Project T.Text deriving (Eq, Show)
newtype Environment = Environment T.Text deriving (Eq, Show)
newtype VarKey = VarKey T.Text deriving (Eq, Show)
newtype VarValue = VarValue T.Text deriving (Eq, Show)

data GitlabConfig = GitlabConfig { gitlabAccessToken :: AccessToken, gitlabApiUrl :: Url } deriving (Eq, Show)

instance FromJSON GitlabConfig where
  parseJSON (Object o) = do
    t <- o .: "access_token"
    u <- o .: "api_url"
    return $ GitlabConfig t u
  parseJSON _ = fail "Expected Object for Config value"


data GitlabProjectDetails = GitlabProjectDetails { gitlabProjectId :: Int
                                     , gitlabProjectName           :: T.Text
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
                                 , gitlabIsueGroup           :: T.Text
                                 , gitlabIssueProject        :: T.Text
                                 , gitlabIssueTitle          :: T.Text
                                 , gitlabIssueDescription    :: T.Text
                                 , gitlabIssueUrl            :: Url
                                 } deriving (Eq, Show)

instance FromJSON GitlabIssueDetails where
  parseJSON (JSON.Object o) = do
    issueId <- o .: "id"
    issueTitle <- o .: "title"
    issueDescription <- o .: "description"
    (Url url) <- o .: "web_url"
    let splits = T.splitOn "/" url
    let groupName = splits !! 3
    let projectName = splits !! 4
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
  parseJSON (JSON.String s)              = fail $ T.unpack $ "Unkown Todo Type: " <> s
  parseJSON _              = fail $ T.unpack $ "Invalid value for Todo. Expecting String."

instance ToJSON GitlabTodoType where
  toJSON GitlabTodoIssue        = "Issue"
  toJSON GitlabTodoMergeRequest = "MergeRequest"

data GitlabTodoDetails = GitlabTodoDetails { gitlabTodoId :: Int
                               , gitlabTodoType           :: GitlabTodoType
                               , gitlabTodoGroup          :: T.Text
                               , gitlabTodoProject        :: T.Text
                               , gitlabTodoTitle          :: T.Text
                               , gitlabTodoDescription    :: T.Text
                               , gitlabTodoUrl            :: Url
                               } deriving (Eq, Show)

instance FromJSON GitlabTodoDetails where
  parseJSON (Object o) = do
    todoId <- o .: "id"
    todoType <- o .: "target_type"
    todoTitle <- (o .: "target") >>= (.: "title")
    todoDescription <- (o .: "target") >>= (.: "description")
    (Url url) <- (o .: "target_url")
    let splits = T.splitOn "/" url
    let groupName = splits !! 3
    let projectName = splits !! 4
    return $ GitlabTodoDetails todoId todoType groupName projectName todoTitle todoDescription (Url url)
  parseJSON _ = fail "Expected Object for GitlabTodoDetails value"

instance ToJSON GitlabTodoDetails where
  toJSON (GitlabTodoDetails i ty g p t d u) = object [ "id" .= i
                                       , "group_name" .= g
                                       , "project_name" .= p
                                       , "title" .= t
                                       , "description" .= d
                                       , "type" .= toJSON ty
                                       , "url" .= toJSON u
                                       ]

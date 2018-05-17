{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab.Types where


import           Data.Aeson  (FromJSON, ToJSON, Value (..), object, parseJSON,
                              toJSON, (.:), (.=))
import qualified Data.Aeson  as JSON
import           Data.Monoid ((<>))
import           Data.Text   as T
import qualified Data.Yaml   as Y

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

instance ToJSON Url where
  toJSON (Url s) = toJSON s

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

instance ToJSON ProjectDetails where
  toJSON (ProjectDetails i n u) = object [ "id" .= i
                                         , "path_with_namespace" .= n
                                         , "web_url" .= toJSON u
                                         ]

data IssueDetails = IssueDetails { issueId           :: Int
                                 , issueProjectGroup :: String
                                 , issueProjectName  :: String
                                 , issueTitle        :: String
                                 , issueDescription  :: String
                                 , issueUrl          :: Url
                                 } deriving (Eq, Show)

instance FromJSON IssueDetails where
  parseJSON (Object o) = do
    issueId <- o .: "id"
    issueTitle <- o .: "title"
    issueDescription <- o .: "description"
    (Url url) <- o .: "web_url"
    let splits = T.splitOn "/" $ T.pack url
    let groupName = T.unpack $ splits !! 3
    let projectName = T.unpack $ splits !! 4
    return $ IssueDetails issueId groupName projectName issueTitle issueDescription (Url url)
  parseJSON _ = fail "Expected Object for IssueDetails value"

instance ToJSON IssueDetails where
  toJSON (IssueDetails i g p t d u) = object [ "id" .= i
                                       , "group_name" .= g
                                       , "project_name" .= p
                                       , "title" .= t
                                       , "description" .= d
                                       , "web_url" .= toJSON u
                                       ]


data TodoType = TodoIssue | TodoMergeRequest deriving (Eq, Show)

instance FromJSON TodoType where
  parseJSON (JSON.String "Issue")        = return TodoIssue
  parseJSON (JSON.String "MergeRequest") = return TodoMergeRequest
  parseJSON (JSON.String s)              = fail $ "Unkown Todo Type: " <> (T.unpack s)

instance ToJSON TodoType where
  toJSON TodoIssue        = "Issue"
  toJSON TodoMergeRequest = "MergeRequest"

data TodoDetails = TodoDetails { todoId           :: Int
                               , todoType         :: TodoType
                               , todoProjectGroup :: String
                               , todoProjectName  :: String
                               , todoTitle        :: String
                               , todoDescription  :: String
                               , todoUrl          :: Url
                               } deriving (Eq, Show)

instance FromJSON TodoDetails where
  parseJSON (Object o) = do
    todoId <- o .: "id"
    todoType <- o .: "target_type"
    todoTitle <- (o .: "target") >>= (.: "title")
    todoDescription <- (o .: "target") >>= (.: "description")
    (Url url) <- (o .: "target") >>= (.: "web_url")
    let splits = T.splitOn "/" $ T.pack url
    let groupName = T.unpack $ splits !! 3
    let projectName = T.unpack $ splits !! 4
    return $ TodoDetails todoId todoType groupName projectName todoTitle todoDescription (Url url)
  parseJSON _ = fail "Expected Object for TodoDetails value"

instance ToJSON TodoDetails where
  toJSON (TodoDetails i ty g p t d u) = object [ "id" .= i
                                       , "group_name" .= g
                                       , "project_name" .= p
                                       , "title" .= t
                                       , "description" .= d
                                       , "type" .= toJSON ty
                                       , "web_url" .= toJSON u
                                       ]

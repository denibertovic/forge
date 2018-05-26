{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab.Lib where

import           RIO

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Simple        (setRequestHeader,
                                             setRequestQueryString)
import           Prelude                    (print)
import           System.Exit                (die)

import qualified Data.Text.Encoding         as TE
import           Forge.Gitlab.Options
import           Forge.Gitlab.Types
import           Forge.HTTP                 (execRequest)
import           Forge.Types                (AccessToken (..), Url (..))
import           Forge.Utils                (addUrlPaths, readConfig)
import qualified RIO.Text                   as T

entrypoint :: GitlabOpts -> IO ()
entrypoint (GitlabOpts config cmd) = do
  c' <- readConfig config
  case cmd of
    CreateVariable g p e k v -> createVariable c' g p e k v
    UpdateVariable g p e k v -> updateVariable c' g p e k v
    DeleteVariable g p e k   -> deleteVariable c' g p e k
    ListProjects             -> listProjects c'
    ListIssues               -> listIssues c'
    ListTodos                -> listTodos c'

createVariable :: GitlabConfig -> Group -> Project ->  Environment -> VarKey -> VarValue -> IO ()
createVariable c (Group g) (Project p) (Environment e) (VarKey k) (VarValue v) = do
  let url = addUrlPaths (gitlabApiUrl c) ["projects", g, "%2F", p, "variables"]
  let method = "POST"
  let pairs =
          [ ("key", TE.encodeUtf8 k)
          , ("value", TE.encodeUtf8 v)
          , ("environment_scope", TE.encodeUtf8 e)
          ]
  initialRequest <- mkInitRequest url (gitlabAccessToken c) method
  ret <- execRequest (urlEncodedBody pairs initialRequest)
  print ret

updateVariable :: GitlabConfig -> Group -> Project -> Environment -> VarKey -> VarValue -> IO ()
updateVariable c (Group g) (Project p) (Environment e) (VarKey k) (VarValue v) = do
  let url = addUrlPaths (gitlabApiUrl c) ["projects", g, "%2F", p, "variables", k]
  let method = "PUT"
  let pairs =
          [ ("value", TE.encodeUtf8 v)
          , ("environment_scope", TE.encodeUtf8 e)
          ]
  initialRequest <- mkInitRequest url (gitlabAccessToken c) method
  ret <- execRequest (urlEncodedBody pairs initialRequest)
  print ret

-- TODO: Add environment scope here once the API supports it
deleteVariable :: GitlabConfig -> Group -> Project -> Environment -> VarKey -> IO ()
deleteVariable c (Group g) (Project p) (Environment _) (VarKey k) = do
  let url = addUrlPaths (gitlabApiUrl c) ["projects", g, "%2F", p, "variables", k]
  let method = "DELETE"
  initialRequest <- mkInitRequest url (gitlabAccessToken c) method
  ret <- execRequest initialRequest
  print ret

listProjects :: GitlabConfig -> IO ()
listProjects c = do
  ret <- listProjects' c
  case ret of
    Left err -> die (show err)
    Right ps -> L8.putStrLn (JSON.encode ps)

listProjects' :: GitlabConfig -> IO (Either String [GitlabProjectDetails])
listProjects' c = do
  let url = addUrlPaths (gitlabApiUrl c) ["projects"]
  let method = "GET"
  initialRequest <- mkInitRequest url (gitlabAccessToken c) method
  let req = setRequestQueryString [("membership", Just "true")] $ initialRequest
  ret <- execRequest req
  let decoded = JSON.eitherDecode' ret :: Either String [GitlabProjectDetails]
  return decoded

listIssues :: GitlabConfig -> IO ()
listIssues c = do
  ret <- listIssues' c
  case ret of
    Left err -> die (show err)
    Right ps -> L8.putStrLn (JSON.encode ps)

listIssues' :: GitlabConfig -> IO (Either String [GitlabIssueDetails])
listIssues' c = do
  let url = addUrlPaths (gitlabApiUrl c) ["issues"]
  let method = "GET"
  initialRequest <- mkInitRequest url (gitlabAccessToken c) method
  let req = setRequestQueryString [("scope", Just "assigned-to-me"), ("state", Just "opened")] $ initialRequest
  ret <- execRequest req
  let decoded = JSON.eitherDecode' ret :: Either String [GitlabIssueDetails]
  return decoded

listTodos :: GitlabConfig -> IO ()
listTodos c = do
  ret <- listTodos' c
  case ret of
    Left err -> die (show err)
    Right ps -> L8.putStrLn (JSON.encode ps)

listTodos' :: GitlabConfig -> IO (Either String [GitlabTodoDetails])
listTodos' c = do
  let url = addUrlPaths (gitlabApiUrl c) ["todos"]
  let method = "GET"
  initialRequest <- mkInitRequest url (gitlabAccessToken c) method
  let req = setRequestQueryString [("state", Just "pending"), ("action", Just "assigned")] $ initialRequest
  ret <- execRequest req
  let decoded = JSON.eitherDecode' ret :: Either String [GitlabTodoDetails]
  return decoded

mkInitRequest :: Url -> AccessToken -> String -> IO Request
mkInitRequest (Url url) (AccessToken t) m = do
  initialRequest <- parseRequest $ T.unpack url
  let request = setRequestHeader "Content-Type" ["application/json"] $ setRequestHeader "PRIVATE-TOKEN" [TE.encodeUtf8 t] $ initialRequest { method = C8.pack m }
  return request

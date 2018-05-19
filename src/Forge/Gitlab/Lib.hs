{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab.Lib where


import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Monoid                ((<>))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple        (getResponseBody, setRequestHeader,
                                             setRequestQueryString)
import           Network.HTTP.Types.Status  (statusCode)
import           System.Exit                (die)
import           Text.Pretty.Simple         (pPrint)

import           Forge.Gitlab.Options
import           Forge.Gitlab.Types
import           Forge.HTTP                 (execRequest)
import           Forge.Types                (AccessToken (..), Url (..))
import           Forge.Utils                (readConfig)

entrypoint :: GitlabOpts -> IO ()
entrypoint (GitlabOpts config cmd) = do
  c' <- readConfig config
  let token = accessToken c'
  case cmd of
    CreateVariable g p e k v -> createVariable token g p e k v
    UpdateVariable g p e k v -> updateVariable token g p e k v
    DeleteVariable g p e k   -> deleteVariable token g p e k
    ListProjects             -> listProjects token
    ListIssues               -> listIssues token
    ListTodos                -> listTodos token

createVariable :: AccessToken -> Group -> Project ->  Environment -> VarKey -> VarValue -> IO ()
createVariable t (Group g) (Project p) (Environment e) (VarKey k) (VarValue v) = do
  let url = Url $ "https://gitlab.com/api/v4/projects/" <> g <> "%2F" <> p <> "/variables/"
  let method = "POST"
  let pairs =
          [ ("key", C8.pack k)
          , ("value", C8.pack v)
          , ("environment_scope", C8.pack e)
          ]
  initialRequest <- mkInitRequest url t method
  ret <- execRequest (urlEncodedBody pairs initialRequest)
  print ret

updateVariable :: AccessToken -> Group -> Project -> Environment -> VarKey -> VarValue -> IO ()
updateVariable t (Group g) (Project p) (Environment e) (VarKey k) (VarValue v) = do
  let url = Url $ "https://gitlab.com/api/v4/projects/" <> g <> "%2F" <> p <> "/variables/" <> k
  let method = "PUT"
  let pairs =
          [ ("value", C8.pack v)
          , ("environment_scope", C8.pack e)
          ]
  initialRequest <- mkInitRequest url t method
  ret <- execRequest (urlEncodedBody pairs initialRequest)
  print ret

-- TODO: Add environment scope here once the API supports it
deleteVariable :: AccessToken -> Group -> Project -> Environment -> VarKey -> IO ()
deleteVariable t (Group g) (Project p) (Environment e) (VarKey k) = do
  let url = Url $ "https://gitlab.com/api/v4/projects/" <> g <> "%2F" <> p <> "/variables/" <> k
  let method = "DELETE"
  initialRequest <- mkInitRequest url t method
  ret <- execRequest initialRequest
  print ret

listProjects :: AccessToken -> IO ()
listProjects t = do
  ret <- listProjects' t
  case ret of
    Left err -> die (show err)
    Right ps -> L8.putStrLn (JSON.encode ps)

listProjects' :: AccessToken -> IO (Either String [ProjectDetails])
listProjects' t = do
  let url = Url $ "https://gitlab.com/api/v4/projects"
  let method = "GET"
  initialRequest <- mkInitRequest url t method
  let req = setRequestQueryString [("membership", Just "true")] $ initialRequest
  ret <- execRequest req
  let decoded = JSON.eitherDecode' ret :: Either String [ProjectDetails]
  return decoded

listIssues :: AccessToken -> IO ()
listIssues t = do
  ret <- listIssues' t
  case ret of
    Left err -> die (show err)
    Right ps -> L8.putStrLn (JSON.encode ps)

listIssues' :: AccessToken -> IO (Either String [IssueDetails])
listIssues' t = do
  let url = Url $ "https://gitlab.com/api/v4/issues"
  let method = "GET"
  initialRequest <- mkInitRequest url t method
  let req = setRequestQueryString [("scope", Just "assigned-to-me"), ("state", Just "opened")] $ initialRequest
  ret <- execRequest req
  let decoded = JSON.eitherDecode' ret :: Either String [IssueDetails]
  return decoded

listTodos :: AccessToken -> IO ()
listTodos t = do
  ret <- listTodos' t
  case ret of
    Left err -> die (show err)
    Right ps -> L8.putStrLn (JSON.encode ps)

listTodos' :: AccessToken -> IO (Either String [TodoDetails])
listTodos' t = do
  let url = Url $ "https://gitlab.com/api/v4/todos"
  let method = "GET"
  initialRequest <- mkInitRequest url t method
  let req = setRequestQueryString [("state", Just "pending"), ("action", Just "assigned")] $ initialRequest
  ret <- execRequest req
  let decoded = JSON.eitherDecode' ret :: Either String [TodoDetails]
  return decoded

mkInitRequest :: Url -> AccessToken -> String -> IO Request
mkInitRequest (Url url) (AccessToken t) m = do
  initialRequest <- parseRequest url
  let request = setRequestHeader "Content-Type" ["application/json"] $ setRequestHeader "PRIVATE-TOKEN" [C8.pack t] $ initialRequest { method = C8.pack m }
  return request

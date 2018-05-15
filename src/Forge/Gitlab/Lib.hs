{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab.Lib where


import           Control.Exception          (try)
import           Control.Monad              (when)
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Monoid                ((<>))
import           Data.Yaml                  (decodeFileEither)
import qualified Data.Yaml                  as Y
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple        (getResponseBody, setRequestHeader,
                                             setRequestQueryString)
import           Network.HTTP.Types.Status  (statusCode)
import           System.Directory           (doesFileExist, makeAbsolute)
import           System.Exit                (die)
import           Text.Pretty.Simple         (pPrint)

import           Forge.Gitlab.Options
import           Forge.Gitlab.Types

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

execRequest :: Request -> IO (L8.ByteString)
execRequest r = do
  manager <- newManager tlsManagerSettings
  response <- try $ httpLbs r manager
  case response of
    Left e         -> die (show (e :: HttpException))
    Right response -> return $ getResponseBody response

mkInitRequest :: Url -> AccessToken -> String -> IO Request
mkInitRequest (Url url) (AccessToken t) m = do
  initialRequest <- parseRequest url
  let request = setRequestHeader "Content-Type" ["application/json"] $ setRequestHeader "PRIVATE-TOKEN" [C8.pack t] $ initialRequest { method = C8.pack m }
  return request

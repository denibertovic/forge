{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}

module Forge.Github.Lib where


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

import           Forge.Github.Options
import           Forge.Github.Types
import           Forge.HTTP                 (execRequest)
import           Forge.Types                (AccessToken (..), Url (..))
import           Forge.Utils                (readConfig)

mkInitRequest :: Url -> AccessToken -> String -> IO Request
mkInitRequest (Url url) (AccessToken t) m = do
  initialRequest <- parseRequest url
  let request = setRequestHeader "Content-Type" ["application/json"] $
                setRequestHeader "User-Agent" [C8.pack "haskell http-client"] $
                setRequestHeader "Authorization" [C8.pack ("token " <> t)] $
                initialRequest { method = C8.pack m }
  return request

entrypoint :: GithubOpts -> IO ()
entrypoint (GithubOpts config cmd) = do
  c' <- readConfig config
  let t =  githubAccessToken c'
  case cmd of
    ListIssues       -> listIssues t
    ListPullRequests -> print "prs"

listIssues :: AccessToken -> IO ()
listIssues t = do
  ret <- listIssues' t
  case ret of
    Left err -> die (show err)
    Right ps -> L8.putStrLn (JSON.encode ps)

listIssues' :: AccessToken -> IO (Either String [IssueDetails])
listIssues' t = do
  let url = Url $ "https://api.github.com/user/issues"
  let method = "GET"
  initialRequest <- mkInitRequest url t method
  let req = setRequestQueryString [("state", Just "open"), ("filter", Just "assigned")] $ initialRequest
  ret <- execRequest req
  let decoded = JSON.eitherDecode' ret :: Either String [IssueDetails]
  return decoded

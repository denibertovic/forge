{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Forge.Github.Lib where

import RIO

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
import Network.HTTP.Simple (setRequestHeader, setRequestQueryString)
import qualified RIO.Text as T
import System.Exit (die)

import qualified Data.Text.Encoding as TE
import Forge.Github.Options
import Forge.Github.Types
import Forge.HTTP (execRequest)
import Forge.Types (AccessToken(..), Url(..))
import Forge.Utils (addUrlPaths, readConfig)

mkInitRequest :: Url -> AccessToken -> String -> IO Request
mkInitRequest (Url url) (AccessToken t) m = do
  initialRequest <- parseRequest $ T.unpack url
  let request =
        setRequestHeader "Content-Type" ["application/json"] $
        setRequestHeader "User-Agent" [C8.pack "haskell http-client"] $
        setRequestHeader "Authorization" [TE.encodeUtf8 ("token " <> t)] $
        initialRequest {method = C8.pack m}
  return request

entrypoint :: GithubOpts -> IO ()
entrypoint (GithubOpts config cmd) = do
  c' <- readConfig config
  case cmd of
    ListIssues -> listIssues c'

listIssues :: GithubConfig -> IO ()
listIssues c = do
  ret <- listIssues' c
  case ret of
    Left err -> die (show err)
    Right ps -> L8.putStrLn (JSON.encode ps)

listIssues' :: GithubConfig -> IO (Either String [GithubIssueDetails])
listIssues' c = do
  let url = addUrlPaths (githubApiUrl c) ["issues"]
  let method = "GET"
  initialRequest <- mkInitRequest url (githubAccessToken c) method
  let req =
        setRequestQueryString
          [("state", Just "open"), ("filter", Just "assigned")] $
        initialRequest
  ret <- execRequest req
  let decoded = JSON.eitherDecode' ret :: Either String [GithubIssueDetails]
  return decoded

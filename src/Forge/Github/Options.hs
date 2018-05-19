{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Github.Options where


import           Control.Monad       (join)
import           Data.Maybe          (fromJust)
import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Forge.Github.Types

data GithubCommand = ListIssues
                   | ListPullRequests

data GithubOpts = GithubOpts {
                   configFilePath :: FilePath
                 , cmd            :: GithubCommand}

type Env = [(String, String)]

environ :: (Show a, HasValue f) => (String -> Maybe a) -> String -> Env -> Mod f a
environ r k env = maybe idm value $ r =<< lookup k env

configPathOpt = strOption
        ( long "config"
        <> short 'c'
        <> metavar "path"
        <> help "absolute path to the config file" )

githubCmds env = subparser (cmdListIssues env <> cmdListPullRequests env)

githubOpts :: Env -> Parser GithubOpts
githubOpts env = GithubOpts <$> configPathOpt <*> githubCmds env

cmdListIssues env = command "my-issues" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "List Github issues"
          options = pure ListIssues

cmdListPullRequests env = command "my-prs" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "List Github todos"
          options = pure ListPullRequests

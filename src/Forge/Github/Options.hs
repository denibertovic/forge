{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Github.Options where

import           RIO

import           Data.Semigroup      ((<>))
import           Options.Applicative

-- import           Forge.Github.Types

data GithubCommand = ListIssues

data GithubOpts = GithubOpts {
                   configFilePath :: FilePath
                 , cmd            :: GithubCommand}

type Env = [(String, String)]

configPathOpt :: Parser FilePath
configPathOpt = strOption
        ( long "config"
        <> short 'c'
        <> metavar "path"
        <> help "absolute path to the config file" )

githubCmds :: Env -> Parser GithubCommand
githubCmds env = subparser (cmdListIssues env)

githubOpts :: Env -> Parser GithubOpts
githubOpts env = GithubOpts <$> configPathOpt <*> githubCmds env

cmdListIssues :: Env -> Mod CommandFields GithubCommand
cmdListIssues _ = command "my-issues" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "List Github issues"
          options = pure ListIssues

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Lib where


import           Data.Semigroup       ((<>))
import           Options.Applicative
import           System.Exit          (ExitCode (..), die, exitWith)

import qualified Forge.Github.Lib     as Github
import           Forge.Github.Options (GithubOpts, githubOpts)
import qualified Forge.Gitlab.Lib     as Gitlab
import           Forge.Gitlab.Options (Env, GitlabOpts, gitlabOpts)

data ForgeCommand = Gitlab GitlabOpts | Github GithubOpts

data ForgeOpts = ForgeOpts {
                   debug :: Bool
                 , cmd   :: ForgeCommand}

debugOpt = switch
        ( long "debug"
        <> help "Debug mode. Verbose output." )

cmdGitlab env = command "gitlab" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Gitlab commands"
          options = Gitlab <$> gitlabOpts env

cmdGithub env = command "github" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Github commands"
          options = Github <$> githubOpts env

forgeCmds env = subparser (cmdGitlab env <> cmdGithub env)

forgeOpts :: Env -> Parser ForgeOpts
forgeOpts env = ForgeOpts <$> debugOpt <*> (forgeCmds env)

entrypoint :: ForgeOpts -> IO ()
entrypoint (ForgeOpts debug cmd) = do
  case cmd of
    Gitlab opts -> Gitlab.entrypoint opts
    Github opts -> Github.entrypoint opts


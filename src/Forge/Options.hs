{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Options where

import           RIO

import           Data.Semigroup       ((<>))
import Data.Version (showVersion)
import Paths_forge (version)
import           Options.Applicative

import           Forge.Github.Options (GithubOpts, githubOpts)
import           Forge.Gitlab.Options (Env, GitlabOpts, gitlabOpts)
import Forge.Types
import qualified Data.Text as T

data ForgeCommand = Gitlab GitlabOpts | Github GithubOpts | Fetch (Maybe MakefileTemplateName)

data ForgeOpts = ForgeOpts {
                   debug :: Bool
                 , cmd   :: ForgeCommand}

debugOpt :: Parser Bool
debugOpt = switch
        ( long "debug"
        <> help "Debug mode. Verbose output." )

versionOpt :: Parser (a -> a)
versionOpt =
  infoOption
    (showVersion version)
    (long "version" <> short 'v' <> help "Show version.")

makeOpt :: Parser (Maybe MakefileTemplateName)
makeOpt =
  optional $
  option (maybeReader readMakefileTemplateName)
    (long "makefile" <> short 'm' <> metavar "NAME" <>
     help "Makefile template to fetch from github repo.")

cmdFetch :: Env -> Mod CommandFields ForgeCommand
cmdFetch _ = command "fetch" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Fetches various templates."
    options = Fetch <$> makeOpt

cmdGitlab :: Env -> Mod CommandFields ForgeCommand
cmdGitlab env = command "gitlab" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Gitlab commands"
          options = Gitlab <$> gitlabOpts env

cmdGithub :: Env -> Mod CommandFields ForgeCommand
cmdGithub env = command "github" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Github commands"
          options = Github <$> githubOpts env

forgeCmds :: Env -> Parser ForgeCommand
forgeCmds env = subparser (cmdGitlab env <> cmdGithub env <> cmdFetch env)

forgeOpts :: Env -> Parser ForgeOpts
forgeOpts env = ForgeOpts <$> debugOpt <*> (forgeCmds env)

readMakefileTemplateName :: String -> Maybe MakefileTemplateName
readMakefileTemplateName m = Just $ MakefileTemplateName $ T.pack m



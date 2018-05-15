{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab.Options where


import           Control.Monad       (join)
import           Data.Maybe          (fromJust)
import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Forge.Gitlab.Types

data GitlabCommand = CreateVariable Group Project Environment VarKey VarValue
                  | UpdateVariable Group Project Environment VarKey VarValue
                  | DeleteVariable Group Project Environment VarKey
                  | ListProjects
                  | ListIssues

data GitlabOpts = GitlabOpts {
                   configFilePath :: FilePath
                 , cmd            :: GitlabCommand}

type Env = [(String, String)]

environ :: (Show a, HasValue f) => (String -> Maybe a) -> String -> Env -> Mod f a
environ r k env = maybe idm value $ r =<< lookup k env

groupOpt env = option (maybeReader readGroup)
        ( long "group"
        <> environ readGroup "GITLAB_GROUP" env
        <> short 'g'
        <> metavar "GITLAB_GROUP"
        <> help "Gitlab Group" )

projectOpt env = option (maybeReader readProject)
        ( long "project"
        <> environ readProject "GITLAB_PROJECT" env
        <> short 'p'
        <> metavar "GITLAB_PROJECT"
        <> help "Gitlab Project" )

configPathOpt = strOption
        ( long "config"
        <> short 'c'
        <> metavar "path"
        <> help "absolute path to the config file" )

gitlabCmds env = subparser (cmdCreateVariable env <> cmdUpdateVariable env <> cmdDeleteVariable env <> cmdListProjects env <> cmdListIssues env)

gitlabOpts :: Env -> Parser GitlabOpts
gitlabOpts env = GitlabOpts <$> configPathOpt <*> gitlabCmds env

readGroup :: String -> Maybe Group
readGroup v = Just $ Group v

readProject :: String -> Maybe Project
readProject v = Just $ Project v

readEnv :: String -> Maybe Environment
readEnv v = Just $ Environment v

readKey :: String -> Maybe VarKey
readKey v = Just $ VarKey v

readVal :: String -> Maybe VarValue
readVal v = Just $ VarValue v

envOpt = option (maybeReader readEnv) (
       long "environment"
       <> short 'e'
       <> metavar "NAME"
       <> help "Name of the environment")

keyOpt = option (maybeReader readKey)(
       long "key"
       <> short 'k'
       <> metavar "KEY"
       <> help "Name of the variable")

valueOpt = option (maybeReader readVal) (
       long "value"
       <> short 'v'
       <> metavar "VALUE"
       <> help "Value of the variable")

cmdCreateVariable env = command "create-var" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Create gitlab variable"
          options = CreateVariable <$> groupOpt env <*> projectOpt env <*> envOpt <*> keyOpt <*> valueOpt

cmdUpdateVariable env = command "update-var" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Create gitlab variable"
          options = UpdateVariable <$> groupOpt env <*> projectOpt env <*> envOpt <*> keyOpt <*> valueOpt

cmdDeleteVariable env = command "delete-var" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Delete gitlab variable"
          options = DeleteVariable <$> groupOpt env <*> projectOpt env <*> envOpt <*> keyOpt

cmdListProjects env = command "list-projects" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "List Gitlab projects"
          options = pure ListProjects

cmdListIssues env = command "list-issues" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "List Gitlab issues"
          options = pure ListIssues

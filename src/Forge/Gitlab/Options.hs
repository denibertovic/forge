{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab.Options where

import           RIO

import           Data.Semigroup      ((<>))
import qualified Data.Text           as T
import           Options.Applicative

import           Forge.Gitlab.Types

data GitlabCommand = CreateVariable Group Project Environment VarKey VarValue
                  | UpdateVariable Group Project Environment VarKey VarValue
                  | DeleteVariable Group Project Environment VarKey
                  | ListProjects
                  | ListIssues
                  | ListTodos

data GitlabOpts = GitlabOpts {
                   configFilePath :: FilePath
                 , cmd            :: GitlabCommand}

type Env = [(String, String)]

environ :: (HasValue f) => (String -> Maybe a) -> String -> Env -> Mod f a
environ r k env = maybe idm value $ r =<< lookup k env

groupOpt :: Env -> Parser Group
groupOpt env = option (maybeReader readGroup)
        ( long "group"
        <> environ readGroup "GITLAB_GROUP" env
        <> short 'g'
        <> metavar "GITLAB_GROUP"
        <> help "Gitlab Group" )

projectOpt :: Env -> Parser Project
projectOpt env = option (maybeReader readProject)
        ( long "project"
        <> environ readProject "GITLAB_PROJECT" env
        <> short 'p'
        <> metavar "GITLAB_PROJECT"
        <> help "Gitlab Project" )

configPathOpt :: Parser FilePath
configPathOpt = strOption
        ( long "config"
        <> short 'c'
        <> metavar "path"
        <> help "absolute path to the config file" )

gitlabCmds :: Env -> Parser GitlabCommand
gitlabCmds env = subparser ( cmdCreateVariable env
                          <> cmdUpdateVariable env
                          <> cmdDeleteVariable env
                          <> cmdListProjects env
                          <> cmdListIssues env
                          <> cmdListTodos env
                           )

gitlabOpts :: Env -> Parser GitlabOpts
gitlabOpts env = GitlabOpts <$> configPathOpt <*> gitlabCmds env

readGroup :: String -> Maybe Group
readGroup v = Just $ Group $ T.pack v

readProject :: String -> Maybe Project
readProject v = Just $ Project $ T.pack v

readEnv :: String -> Maybe Environment
readEnv v = Just $ Environment $ T.pack v

readKey :: String -> Maybe VarKey
readKey v = Just $ VarKey $ T.pack v

readVal :: String -> Maybe VarValue
readVal v = Just $ VarValue $ T.pack v

envOpt :: Parser Environment
envOpt = option (maybeReader readEnv) (
       long "environment"
       <> short 'e'
       <> metavar "NAME"
       <> help "Name of the environment")

keyOpt :: Parser VarKey
keyOpt = option (maybeReader readKey)(
       long "key"
       <> short 'k'
       <> metavar "KEY"
       <> help "Name of the variable")

valueOpt :: Parser VarValue
valueOpt = option (maybeReader readVal) (
       long "value"
       <> short 'v'
       <> metavar "VALUE"
       <> help "Value of the variable")

cmdCreateVariable :: Env -> Mod CommandFields GitlabCommand
cmdCreateVariable env = command "create-var" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Create gitlab variable"
          options = CreateVariable <$> groupOpt env <*> projectOpt env <*> envOpt <*> keyOpt <*> valueOpt

cmdUpdateVariable :: Env -> Mod CommandFields GitlabCommand
cmdUpdateVariable env = command "update-var" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Create gitlab variable"
          options = UpdateVariable <$> groupOpt env <*> projectOpt env <*> envOpt <*> keyOpt <*> valueOpt

cmdDeleteVariable :: Env -> Mod CommandFields GitlabCommand
cmdDeleteVariable env = command "delete-var" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Delete gitlab variable"
          options = DeleteVariable <$> groupOpt env <*> projectOpt env <*> envOpt <*> keyOpt

cmdListProjects :: Env -> Mod CommandFields GitlabCommand
cmdListProjects _ = command "my-projects" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "List Gitlab projects"
          options = pure ListProjects

cmdListIssues :: Env -> Mod CommandFields GitlabCommand
cmdListIssues _ = command "my-issues" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "List Gitlab issues"
          options = pure ListIssues

cmdListTodos :: Env -> Mod CommandFields GitlabCommand
cmdListTodos _ = command "my-todos" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "List Gitlab todos"
          options = pure ListTodos

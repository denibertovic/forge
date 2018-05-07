{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab.Options where


import           Control.Monad       (join)
import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Forge.Gitlab.Types

data GitlabCommand = CreateVariable Environment VarKey VarValue
                  | UpdateVariable Environment VarKey VarValue
                  | DeleteVariable Environment VarKey

data GitlabOpts = GitlabOpts {
                   configFilePath :: FilePath
                 , group          :: String
                 , project        :: String
                 , cmd            :: GitlabCommand}

type Env = [(String, String)]

environ :: (HasValue f) => String -> Env -> Mod f String
environ k env = maybe idm value . join $ parse <$> lookup k env
  where
    parse = either (const Nothing) Just . Right

textOption :: Mod OptionFields String -> Parser String
textOption = option (eitherReader (Right))

groupOpt env = textOption
        ( long "group"
        <> environ "GITLAB_GROUP" env
        <> short 'g'
        <> metavar "GITLAB_GROUP"
        <> help "Gitlab Group." )

projectOpt env = textOption
        ( long "project"
        <> environ "GITLAB_PROJECT" env
        <> short 'p'
        <> metavar "GITLAB_PROJECT"
        <> help "Gitlab Project." )

configPathOpt = strOption
        ( long "config"
        <> short 'c'
        <> metavar "path"
        <> help "absolute path to the config file." )

gitlabCmds = subparser (cmdCreateVariable <> cmdUpdateVariable <> cmdDeleteVariable)

gitlabOpts :: Env -> Parser GitlabOpts
gitlabOpts env = GitlabOpts <$> configPathOpt <*> (groupOpt env) <*> (projectOpt env) <*> gitlabCmds

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
       <> help "Value of the variable.")

cmdCreateVariable = command "create-var" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Create gitlab variable."
          options = CreateVariable <$> envOpt <*> keyOpt <*> valueOpt

cmdUpdateVariable = command "update-var" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Create gitlab variable."
          options = UpdateVariable <$> envOpt <*> keyOpt <*> valueOpt

cmdDeleteVariable = command "delete-var" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Delete gitlab variable."
          options = DeleteVariable <$> envOpt <*> keyOpt

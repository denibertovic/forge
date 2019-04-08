{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Firefox.Options where

import           RIO

import           Options.Applicative
import qualified Data.Text           as T

import Forge.Firefox.Types

type Env = [(String, String)]

data FirefoxCommand = FirefoxProfileCommand ProfileOpts
data FirefoxOpts = FirefoxOpts { fcmd :: FirefoxCommand }

cmdProfile :: Env -> Mod CommandFields FirefoxCommand
cmdProfile env = command "profile" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Firefox profile commands."
    options = FirefoxProfileCommand <$> profileOpts env

firefoxCmds :: Env -> Parser FirefoxCommand
firefoxCmds env = subparser (cmdProfile env)

firefoxOpts :: Env -> Parser FirefoxOpts
firefoxOpts env = FirefoxOpts <$> firefoxCmds env

--- Profile Command

data ProfileCommand = ListProfiles
                    | RemoveProfile FirefoxProfile
                    | NewProfile FirefoxProfile FirefoxProfile

data ProfileOpts = ProfileOpts { pcmd :: ProfileCommand }

profileOpts :: Env -> Parser ProfileOpts
profileOpts env = ProfileOpts <$> profileCmds env

profileCmds :: Env -> Parser ProfileCommand
profileCmds env =
  subparser (cmdNewProfile env <> cmdListProfiles env <> cmdRemoveProfile env)

readProfile :: String -> Maybe FirefoxProfile
readProfile v = Just $ FirefoxProfile $ T.pack v

baseProfileOpt :: Env -> Parser FirefoxProfile
baseProfileOpt _ = option (maybeReader readProfile)
        ( long "base"
        <> short 'b'
        <> metavar "BASE_PROFILE"
        <> value (FirefoxProfile "template")
        <> help "Firefox base (template) profile." )

cmdNewProfile :: Env -> Mod CommandFields ProfileCommand
cmdNewProfile env = command "new" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Make new Firefox profile."
          options = NewProfile <$> baseProfileOpt env <*> argument (maybeReader readProfile) (metavar "PROFILE")

cmdListProfiles :: Env -> Mod CommandFields ProfileCommand
cmdListProfiles _ = command "ls" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "List Firefox profiles"
          options = pure ListProfiles

cmdRemoveProfile :: Env -> Mod CommandFields ProfileCommand
cmdRemoveProfile _ = command "rm" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Remove firefox profile."
          options = RemoveProfile <$> argument (maybeReader readProfile) (metavar "PROFILE")

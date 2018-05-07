{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Lib where


import           Data.Semigroup       ((<>))
import           Options.Applicative
import           System.Exit          (ExitCode (..), die, exitWith)

import qualified Forge.Gitlab.Lib     as Gitlab
import           Forge.Gitlab.Options (Env, GitlabOpts, gitlabOpts)

data ForgeCommand = Gitlab GitlabOpts

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

forgeCmds env = subparser (cmdGitlab env)

forgeOpts :: Env -> Parser ForgeOpts
forgeOpts env = ForgeOpts <$> debugOpt <*> (forgeCmds env)

entrypoint :: ForgeOpts -> IO ()
entrypoint (ForgeOpts debug cmd) = do
  case cmd of
    Gitlab opts -> Gitlab.entrypoint opts


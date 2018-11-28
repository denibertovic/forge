{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Terraform.Options where

import           RIO

import           Options.Applicative

import Forge.Terraform.Types

data TerraformCommand = MakeDocs TerraformVersion

data TerraformOpts = TerraformOpts { cmd :: TerraformCommand}

type Env = [(String, String)]

terraformCmds :: Env -> Parser TerraformCommand
terraformCmds env = subparser (cmdMakeDocs env)

terraformOpts :: Env -> Parser TerraformOpts
terraformOpts env = TerraformOpts <$> terraformCmds env

cmdMakeDocs :: Env -> Mod CommandFields TerraformCommand
cmdMakeDocs _ = command "make-docs" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Fetch terraform docs and convert into manpages."
          options = MakeDocs <$> argument str (metavar "VERSION")

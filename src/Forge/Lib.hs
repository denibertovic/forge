{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Forge.Lib where

import RIO

import qualified Forge.Github.Lib as Github
import qualified Forge.Gitlab.Lib as Gitlab
import qualified Forge.Terraform.Lib as Terraform
import Forge.HTTP (downloadFile)
import Forge.Options
import Forge.Types
import qualified Data.Text as T
import System.Exit (die)
import RIO.Directory (doesFileExist, renameFile)

entrypoint :: ForgeOpts -> IO ()
entrypoint (ForgeOpts _ cmd) = do
  case cmd of
    Gitlab opts -> Gitlab.entrypoint opts
    Github opts -> Github.entrypoint opts
    Fetch makefile -> fetchTemplate makefile
    Terraform opts -> Terraform.entrypoint opts

fetchTemplate :: Maybe MakefileTemplateName -> IO ()
fetchTemplate m = do
  let baseUrl =
        "https://raw.githubusercontent.com/denibertovic/makefiles/master/"
  case m of
    Nothing ->
      die
        "Please specify a template name in the format 'myorg/templatename'. See here for a list of my default templates: https://github.com/denibertovic/makefiles."
    Just (MakefileTemplateName name) -> do
      let (b', n') = case T.splitOn "/" name of
                [_] -> (baseUrl, name)
                [group, name'] -> (T.replace "denibertovic" group baseUrl, name')
      exists <- doesFileExist "Makefile"
      when exists (renameFile "Makefile" "Makefile.old")
      downloadFile (b' <> n' <> ".makefile") "Makefile"

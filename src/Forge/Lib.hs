{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Forge.Lib where

import RIO

import qualified Forge.Github.Lib as Github
import qualified Forge.Gitlab.Lib as Gitlab
import Forge.HTTP (downloadFile)
import Forge.Options
import Forge.Types
import System.Exit (die)
import RIO.Directory (doesFileExist, renameFile)

entrypoint :: ForgeOpts -> IO ()
entrypoint (ForgeOpts _ cmd) = do
  case cmd of
    Gitlab opts -> Gitlab.entrypoint opts
    Github opts -> Github.entrypoint opts
    Fetch makefile -> fetchTemplate makefile

fetchTemplate :: Maybe MakefileTemplateName -> IO ()
fetchTemplate m = do
  let baseUrl =
        "https://raw.githubusercontent.com/denibertovic/makefiles/master/"
  case m of
    Nothing ->
      die
        "Please specify a template name. See here for a list of templates: https://github.com/denibertovic/makefiles"
    Just (MakefileTemplateName name) -> do
      exists <- doesFileExist "Makefile"
      when exists (renameFile "Makefile" "Makefile.old")
      downloadFile (baseUrl <> name <> ".makefile") "Makefile"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Forge.Lib where

import RIO

import qualified Forge.Github.Lib as Github
import qualified Forge.Gitlab.Lib as Gitlab
import qualified Forge.Terraform.Lib as Terraform
import qualified Forge.Firefox.Lib as Firefox
import qualified Forge.Dns.Lib as Dns
import Forge.HTTP (downloadFile)
import Forge.Options
import Forge.Types
import qualified Data.Text as T
import System.Exit (die)
import RIO.Directory (doesFileExist, renameFile)

errMsg :: String
errMsg = "Please specify a template name in the format 'myorg/templatename'. See here for a list of my default templates: https://github.com/denibertovic/makefiles."

entrypoint :: ForgeOpts -> IO ()
entrypoint (ForgeOpts _ cmd) = do
  case cmd of
    Gitlab opts -> Gitlab.entrypoint opts
    Github opts -> Github.entrypoint opts
    Fetch makefile -> fetchTemplate makefile
    Terraform opts -> Terraform.entrypoint opts
    Firefox opts -> Firefox.entrypoint opts
    Dns opts -> Dns.entrypoint opts

fetchTemplate :: Maybe MakefileTemplateName -> IO ()
fetchTemplate m = do
  let baseUrl =
        "https://raw.githubusercontent.com/denibertovic/makefiles/master/"
  case m of
    Nothing ->
      die errMsg
    Just (MakefileTemplateName name) -> do
      let ret = case T.splitOn "/" name of
                [_] -> Just (baseUrl, name)
                [group, name'] -> Just (T.replace "denibertovic" group baseUrl, name')
                _ -> Nothing
      case ret of
        Nothing -> die $ "Failed to parse. " <> errMsg
        Just (b', n') -> do
            exists <- doesFileExist "Makefile"
            when exists (renameFile "Makefile" "Makefile.old")
            downloadFile (b' <> n' <> ".makefile") "Makefile"

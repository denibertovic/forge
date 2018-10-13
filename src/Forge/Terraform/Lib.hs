{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Forge.Terraform.Lib where

import Prelude (putStrLn)
import RIO

import Data.Conduit
import Data.Conduit.Combinators (sourceFileBS)
import qualified Data.Conduit.Tar as Tar
import Data.Conduit.Zlib (ungzip)
import Data.Foldable (for_)
import qualified RIO.Text as T
import System.Directory (copyFile, doesDirectoryExist, removeFile)
import System.Directory (createDirectory, getHomeDirectory)
import System.FilePath ((</>), dropExtension, takeBaseName, takeFileName)
import System.FilePath.Find ((~~?), always, fileName, find)
import System.FilePath.Manip (renameWith)
import System.Process.Typed (readProcess_, runProcess_, shell)

import Forge.HTTP (downloadFile)
import Forge.Terraform.Options
import Forge.Terraform.Types

entrypoint :: TerraformOpts -> IO ()
entrypoint (TerraformOpts cmd) = do
  case cmd of
    MakeDocs v -> makeDocs v

makeDocs :: TerraformVersion -> IO ()
makeDocs v = do
  putStrLn "Fetching provider tarball..."
  let baseUrl =
        "https://github.com/terraform-providers/terraform-provider-aws/archive/"
  home <- getHomeDirectory
  let manDir = home </> ".local" </> "share" </> "man" </> "man7"
  manexists <- doesDirectoryExist manDir
  unless manexists (createDirectory manDir)
  withSystemTempDirectory "tfdocs--" $ \d -> do
    let archivePath = d </> "docs.tar.gz"
    let archiveUrl = baseUrl <> "v" <> T.pack v <> ".tar.gz"
    let outputDir = d </> "out"
    createDirectory outputDir
    createDirectory $ outputDir </> "man"
    downloadFile archiveUrl (T.pack archivePath)
    _ <-
      runConduitRes
        (sourceFileBS archivePath -- read the raw file
          .|
         ungzip -- gunzip
          .|
         Tar.untarWithExceptions (Tar.restoreFileIntoLenient outputDir))
    putStrLn "Generating man files..."
    renameForConsistency (outputDir </> "terraform-provider-aws-" <> v)
    convertMarkdownToManpage
      (outputDir </> "terraform-provider-aws-" <> v)
      outputDir
    cleanOldDocs manDir
    copyManFiles (outputDir </> "man") manDir
    generateManDb
    putStrLn $ "Done. Generated AWS Terraform docs for version: " <> v

-- They commit files with .md and .markdown extensions
-- We try and be consitent so that the finder function doesn't miss anything
renameForConsistency :: FilePath -> IO ()
renameForConsistency dir = do
  fs <- find always (fileName ~~? "*.md") (dir </> "website" </> "docs")
  for_ fs $ \f -> do renameWith (\f' -> dropExtension f' <> ".markdown") f

convertMarkdownToManpage :: FilePath -> FilePath -> IO ()
convertMarkdownToManpage d out = do
  resources <-
    find always (fileName ~~? "*.markdown") (d </> "website" </> "docs" </> "r")
  dataResources <-
    find always (fileName ~~? "*.markdown") (d </> "website" </> "docs" </> "d")
  for_ resources $ \f -> do
    readProcess_ $
      shell $
      "pandoc " <> f <> " -s -t man -o " <>
      (out </> "man" </> "tf_aws_" <>
       (takeBaseName $ takeBaseName $ takeFileName f) <>
       ".7")
  for_ dataResources $ \f -> do
    readProcess_ $
      shell $
      "pandoc " <> f <> " -s -t man -o " <>
      (out </> "man" </> "tf_aws_data_" <>
       (takeBaseName $ takeBaseName $ takeFileName f) <>
       ".7")

cleanOldDocs :: FilePath -> IO ()
cleanOldDocs manDir = do
  fs <- find always (fileName ~~? "tf_aws_*.7") manDir
  for_ fs $ \f -> do removeFile f

copyManFiles :: FilePath -> FilePath -> IO ()
copyManFiles src dest = do
  fs <- find always (fileName ~~? "*.7") src
  for_ fs $ \f -> do copyFile f (dest </> takeFileName f)

generateManDb :: IO ()
generateManDb = do
  runProcess_ $ shell "mandb"

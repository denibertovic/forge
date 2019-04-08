{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Forge.Firefox.Lib where

import Prelude (putStrLn)
import RIO
import RIO.Directory (doesFileExist, copyFile, doesDirectoryExist, getHomeDirectory)

import Data.List (sort, last)
import Text.Read
import Data.Ini (Ini(..), parseIni, writeIniFileWith, WriteIniSettings(..), KeySeparator(..), sections)
import qualified RIO.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import System.Exit (die)

import System.FilePath ((</>))
import System.Process.Typed as TP
import qualified Data.Text.IO as TIO

import Forge.Firefox.Options
import Forge.Firefox.Types

entrypoint :: FirefoxOpts -> IO ()
entrypoint (FirefoxOpts cmd) = do
  case cmd of
    FirefoxProfileCommand (ProfileOpts ListProfiles) -> listProfiles
    FirefoxProfileCommand (ProfileOpts (RemoveProfile p)) -> removeProfile p
    FirefoxProfileCommand (ProfileOpts (NewProfile b p)) -> newProfile b p

listProfiles :: IO ()
listProfiles = do
  c <- parseIniOrDie
  forM_ (zip ([0..] :: [Int]) (concatMap toList $ pairs c)) $ \(i, n) -> do
    putStrLn $ show i <> ". " <> (T.unpack n)
  where pairs :: Ini -> HashMap Text (Maybe Text)
        pairs i = fmap (HM.lookup "Name") (unIni i)

newProfile :: FirefoxProfile -> FirefoxProfile -> IO ()
newProfile (FirefoxProfile b) (FirefoxProfile p) = do
  h <- getHomeDirectory
  f <- getIniFileOrDie
  ini <- parseIniOrDie
  let un = unIni ini
  let base = findProfile (FirefoxProfile b) ini
  case base of
    Just (_, path, isRel) -> do
      let pFullName = mkName (FirefoxProfile p)
      putStrLn $
        "Making new profile " <> "'" <> (T.unpack p) <> "'" <> " from base profile: " <>
        (T.unpack b)
      let b' =
            if isRel
              then (mkAbsPath h path)
              else path
      TP.runProcess_ $ TP.shell $ "cp -r " <> b' <> " " <> (mkAbsPath h $ T.unpack pFullName)
      let sectionNums = sort $ map read $ map T.unpack $ concatMap toList $ map (T.stripPrefix "Profile") $ filter (T.isPrefixOf "Profile") $ sections ini :: [Int]
      let newP = HM.fromList [("Name", p), ("Path", pFullName), ("IsRelative", "1")]
      let newUnIni = HM.insert ("Profile" <> (T.pack $ show $ (last sectionNums) + 1)) newP un
      writeIni f (Ini newUnIni)
    Nothing -> die $ ("Invalid base profile: " <> T.unpack b)

writeIni :: FilePath -> Ini -> IO ()
writeIni f ini = do
    copyFile f (f <> ".backup")
    writeIniFileWith settings f ini
  where settings = WriteIniSettings { writeIniKeySeparator = EqualsKeySeparator}

removeProfile :: FirefoxProfile -> IO ()
removeProfile profile = do
  h <- getHomeDirectory
  f <- getIniFileOrDie
  ini <- parseIniOrDie
  let p = findProfile profile ini
  case p of
    Nothing -> die "Error: Profile does not exist!"
    Just (_, path, r) -> do
      let (Just s) = findSection profile ini -- NOTE: Should not be here since the findProfle case would error
      let path' = if r then mkAbsPath h path else path
      let ini' = HM.delete s (unIni ini)
      writeIni f (Ini ini')
      exists <- doesDirectoryExist path'
      when (exists && isSafe path') (TP.runProcess_ $ TP.shell $ "rm -r " <> path')

isSafe :: FilePath -> Bool
isSafe f = T.isInfixOf ".mozilla/firefox" $ T.pack f

mkAbsPath :: FilePath -> FilePath -> FilePath
mkAbsPath h f = h </> ".mozilla" </> "firefox" </> f

mkName :: FirefoxProfile -> T.Text
mkName (FirefoxProfile p) = p <> ".profile"

parseIniOrDie :: IO (Ini)
parseIniOrDie = do
  f <- getIniFileOrDie
  config <- TIO.readFile f
  let i = parseIni config
  either (\_ -> die "ERROR: Failed parsing profile.ini") return i

getIniFileOrDie :: IO (FilePath)
getIniFileOrDie = do
  h <- getHomeDirectory
  let f = h </> ".mozilla" </> "firefox" </> "profiles.ini"
  exists <- doesFileExist f
  unless exists (die $ "ERROR: profiles.ini does not exist: " <> f)
  return f

findSection :: FirefoxProfile -> Ini -> Maybe Text
findSection p ini = findSection' p (HM.toList $ pairs ini)
  where
    pairs :: Ini -> HashMap Text (Maybe Text)
    pairs i =
      fmap (HM.lookup "Name") (unIni i)
    findSection' :: FirefoxProfile -> [(Text, Maybe Text)] -> Maybe Text
    findSection' _ [] = Nothing
    findSection' (FirefoxProfile p) ((section, name):xs) =
      case name of
        Nothing -> findSection' (FirefoxProfile p) xs
        Just n ->
          if n == p
            then Just section
            else findSection' (FirefoxProfile p) xs

findProfile :: FirefoxProfile -> Ini ->  Maybe (T.Text, FilePath, Bool)
findProfile p ini = findProfile' p (HM.toList $ pairs ini)
  where
    pairs :: Ini -> HashMap Text (Maybe Text, Maybe Text, Maybe Text)
    pairs i =
      fmap
        (\x ->
           (HM.lookup "Name" x, HM.lookup "Path" x, HM.lookup "IsRelative" x))
        (unIni i)
    findProfile' :: FirefoxProfile -> [(Text, (Maybe Text, Maybe Text, Maybe Text))] -> Maybe (T.Text, FilePath, Bool)
    findProfile' _ [] = Nothing
    findProfile' (FirefoxProfile p) ((_, (name, path, isrel)):xs) =
      case (name, path, isrel) of
        (Nothing, _, _) -> findProfile' (FirefoxProfile p) xs
        (Just n, Just path', Just rel) ->
          if n == p
            then Just
                   ( n
                   , T.unpack path'
                   , if rel == "1"
                       then True
                       else False)
            else findProfile' (FirefoxProfile p) xs
        _ -> findProfile' (FirefoxProfile p) xs

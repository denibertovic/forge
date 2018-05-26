{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Utils where

import           RIO

import           Control.Monad (when)
import           Data.Aeson    (FromJSON)
import           Data.Text     as T
import qualified Data.Yaml     as Y
import           RIO.Directory (doesFileExist)
import           System.Exit   (die)

import           Forge.Types   (Url (..))

decodeConfig :: FromJSON a => FilePath -> IO (Either Y.ParseException a)
decodeConfig p = Y.decodeFileEither p

readConfig :: FromJSON a => FilePath -> IO a
readConfig p = do
  exists <- doesFileExist p
  when (not exists) (die "Config file does not exist.")
  c <- decodeConfig p
  case c of
    Left err -> die (show err)
    Right c' -> return c'

slash :: T.Text
slash = "/"

addUrlPaths :: Url -> [T.Text] -> Url
addUrlPaths (Url u) as = Url $ (dropLastSlash u) <> slash <> (parts as)
  where parts xs = (T.intercalate slash xs)

dropLastSlash :: T.Text -> T.Text
dropLastSlash xs = T.dropWhileEnd (== '/') xs

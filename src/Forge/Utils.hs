{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Utils where

import           Control.Monad    (when)
import           Data.Aeson       (FromJSON)
import           Data.Yaml        (decodeFileEither)
import qualified Data.Yaml        as Y
import           System.Directory (doesFileExist, makeAbsolute)
import           System.Exit      (die)

decodeConfig :: FromJSON a => FilePath -> IO (Either Y.ParseException a)
decodeConfig p = Y.decodeFileEither p

readConfig :: FromJSON a => FilePath -> IO a
readConfig p = do
  exists <- doesFileExist p
  when (not exists) (die "Config file does not exist.")
  c <- decodeConfig p
  case c of
    Left err -> die (show err)
    Right c  -> return c

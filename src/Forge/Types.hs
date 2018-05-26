{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Types where

import           RIO

import           Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import qualified Data.Aeson as JSON
import qualified RIO.Text   as T

newtype AccessToken = AccessToken T.Text deriving (Eq, Show)
newtype Url = Url T.Text deriving (Eq, Show)

instance FromJSON AccessToken where
  parseJSON (JSON.String s) = return $ AccessToken s
  parseJSON _               = fail "Expected String for AccessToken"

instance ToJSON AccessToken where
  toJSON (AccessToken s) = toJSON s

instance FromJSON Url where
  parseJSON (JSON.String s) = return $ Url s
  parseJSON _               = fail "Expected String for Url"

instance ToJSON Url where
  toJSON (Url s) = toJSON s

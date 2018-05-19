{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Types where


import           Data.Aeson (FromJSON, ToJSON, Value (..), parseJSON, toJSON,
                             (.:))
import qualified Data.Aeson as JSON
import           Data.Text  as T

newtype AccessToken = AccessToken String deriving (Eq, Show)
newtype Url = Url String deriving (Eq, Show)

instance FromJSON Url where
  parseJSON (JSON.String s) = return $ Url $ T.unpack s
  parseJSON _               = fail "Expected String for Url"

instance ToJSON Url where
  toJSON (Url s) = toJSON s

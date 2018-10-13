{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Terraform.Types where

import           RIO

import           Data.Aeson  (FromJSON, ToJSON, Value (..), object, parseJSON,
                              toJSON, (.:), (.:?), (.=))
import           Data.List   ((!!))
import           Data.Text   as T

import           Forge.Types (Url (..))

type TerraformVersion = String


{-# LANGUAGE OverloadedStrings #-}

module Forge.Firefox.Types where

import           RIO hiding (optional, bool)
import qualified Data.Text as T

newtype FirefoxProfile = FirefoxProfile T.Text deriving (Eq, Show)

oneZeroBool :: T.Text -> Either String Bool
oneZeroBool "0" = Right False
oneZeroBool "1" = Right True
oneZeroBool x   = Left $ T.unpack $ "Could not parse: " <> x <> " as Bool."

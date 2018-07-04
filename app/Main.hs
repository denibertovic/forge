module Main where

import RIO

import Data.Semigroup ((<>))
import Options.Applicative
import System.Environment (getEnvironment)

import Forge.Lib
import Forge.Options

main :: IO ()
main = do
  env <- getEnvironment
  execParser (opts env) >>= entrypoint
  where
    opts env =
      info
        (helper <*> versionOpt <*> forgeOpts env)
        (fullDesc <> progDesc "Helper for working with various APIs" <>
         header "forge - A helper tool for with various APIs")

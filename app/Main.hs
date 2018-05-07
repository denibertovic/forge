module Main where

import           Data.Yaml           (decodeFile)
import qualified Data.Yaml           as Y
import           Forge.Lib

import           Data.Semigroup      ((<>))
import qualified Data.Text           as T
import           Options.Applicative
import           System.Environment  (getEnvironment)


main :: IO ()
main = do
  env <- getEnvironment
  execParser (opts env) >>= entrypoint
  where
    opts env = info (helper <*> forgeOpts env)
      ( fullDesc
     <> progDesc "Helper for working with various APIs."
     <> header "forge - A helper tool for with various APIs." )


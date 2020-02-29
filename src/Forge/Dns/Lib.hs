{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Forge.Dns.Lib where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Forge.Dns.Options
import Forge.HTTP (execRequest)
import RIO
import System.IO.Error (isDoesNotExistError)
import qualified Forge.Dns.DigitalOcean as DO

entrypoint :: DnsOpts -> IO ()
entrypoint (DnsOpts cmd) = do
  case cmd of
    DnsUpdate provider names ip -> case provider of
      DigitalOceanDns -> DO.updateDns cacheFile checkCache names ip

cacheFile :: FilePath
cacheFile = "/tmp/cachedip"

checkCache :: BS.ByteString -> IO (Either String Text)
checkCache ip = do
  cachedIp <- tryJust (guard . isDoesNotExistError) $ BS.readFile cacheFile
  case cachedIp of
    Left _ -> do
      -- no cache so we have to try and update
      return $ Right $ decodeUtf8 ip
    Right cip -> do
      if (ip == cip)
        then do
          return $ Left "IP did not change"
        else do
          return $ Right $ decodeUtf8 ip

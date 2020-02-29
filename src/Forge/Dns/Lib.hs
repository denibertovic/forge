{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Forge.Dns.Lib where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Forge.Dns.Options
import Forge.HTTP (execRequest)
import qualified Network.DigitalOcean as D
import Network.DigitalOcean.Types (DO(..), Client(..))
import Network.DigitalOcean.Services.DomainRecord (DomainRecord(..), DomainRecordPayload(..))
import Network.HTTP.Client (parseRequest)
import RIO
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import System.Exit (die)
import System.IO.Error (isDoesNotExistError)
import Prelude (print)
import Forge.Utils (lookupSettingThrow)

entrypoint :: DnsOpts -> IO ()
entrypoint (DnsOpts cmd) = do
  case cmd of
    DnsUpdate name ip -> updateDns name ip

client :: Text -> Client
client key = Client $ encodeUtf8 key

updateDns :: Text -> Maybe Text -> IO ()
updateDns name mip = do
  key <- lookupSettingThrow "DO_TOKEN"
  req <- parseRequest "http://ip.42.pl/raw"
  ip <- execRequest req
  r <- checkCache ip
  case r of
    -- IP did not change
    Left _ -> exitSuccess
    -- IP changed
    Right newIp -> do
      result <- runExceptT $ flip runReaderT (client key) $ runDO $ updateDomain name newIp
      case result of
        Left err -> die $ show err
        Right _ -> return ()

updateDomain :: Text -> Text -> DO ()
updateDomain name ip = do
  let payload = DomainRecordPayload
        { domainrecordpayloadType = "A",
          domainrecordpayloadName = Just $ T.unpack $ T.intercalate "" $ take 1 $ T.splitOn "." name,
          domainrecordpayloadData = Just $ T.unpack ip,
          domainrecordpayloadPriority = Nothing,
          domainrecordpayloadPort = Nothing,
          domainrecordpayloadTtl = Just 60,
          domainrecordpayloadWeight = Nothing
        }
  domains <- D.getDomainRecords $ (T.unpack $ takeLast 2 name)
  let found = filter (\x -> (domainrecordName x) == (T.unpack $ T.intercalate "" $ take 1 $ T.splitOn "." name)) domains
  case (listToMaybe found) of
    Nothing -> do
      D.createDomainRecord (T.unpack $ takeLast 2 name) payload
      return ()
    Just d -> do
      D.updateDomainRecord (T.unpack $ takeLast 2 name) (domainrecordId d) payload
      return ()
  where
    takeLast x name = T.intercalate "." . reverse . take x . reverse $ T.splitOn "." name

checkCache :: BSL.ByteString -> IO (Either String Text)
checkCache ip = do
  let cacheFile = "/tmp/ip"
  cachedIp <- tryJust (guard . isDoesNotExistError) $ BSL.readFile cacheFile
  case cachedIp of
    Left _ -> do
      BSL.writeFile cacheFile ip
      -- no cache so we have to try and update
      return $ Right $ decodeUtf8 $ BSL.toStrict ip
    Right cip -> do
      if (ip == cip)
        then do
          return $ Left "IP did not change"
        else do
          BSL.writeFile cacheFile ip
          return $ Right $ decodeUtf8 $ BSL.toStrict ip

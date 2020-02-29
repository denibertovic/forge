{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Forge.Dns.DigitalOcean where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Forge.HTTP (execRequest)
import qualified Network.DigitalOcean as D
import Network.DigitalOcean.Types (DO(..), Client(..))
import Network.DigitalOcean.Services.DomainRecord (DomainRecord(..), DomainRecordPayload(..))
import Network.HTTP.Client (parseRequest)
import RIO
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import System.Exit (die, exitSuccess)
import System.IO.Error (isDoesNotExistError)
import Forge.Utils (lookupSettingThrow)
import System.Directory (removeFile)
import Prelude (print)

client :: Text -> Client
client key = Client $ encodeUtf8 key

updateDns :: FilePath -> (BS.ByteString -> IO (Either String Text)) -> [Text] -> Maybe Text -> IO ()
updateDns cacheFile checkCache names mip = do
  key <- lookupSettingThrow "DO_TOKEN"
  ip <- case mip of
    Nothing -> do
      req <- parseRequest "http://ip.42.pl/raw"
      ip <- execRequest req
      return ip
    Just ip -> return $ BSL.fromStrict $ encodeUtf8 ip
  r <- checkCache (BSL.toStrict ip)
  case r of
    -- IP did not change
    Left _ -> exitSuccess
    -- IP changed
    Right newIp -> do
      for_ names $ \name -> do
        result <- runExceptT $ flip runReaderT (client key) $ runDO $ updateDomain newIp name
        case result of
          Left err -> do
            -- NOTE: this is a hack until I implement a better caching scheme based on each
            -- updated domain. For now we just delete the file so that the second update
            -- will re-try updating all the domains
            removeFile cacheFile
            die $ show err
          Right _ -> do
            BS.writeFile cacheFile (encodeUtf8 newIp)
            return ()

updateDomain :: Text -> Text -> DO ()
updateDomain ip name = do
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

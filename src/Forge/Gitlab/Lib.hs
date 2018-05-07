{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Gitlab.Lib where


import           Control.Exception          (try)
import           Control.Monad              (when)
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Monoid                ((<>))
import           Data.Yaml                  (decodeFileEither)
import qualified Data.Yaml                  as Y
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple        (getResponseBody, setRequestHeader)
import           Network.HTTP.Types.Status  (statusCode)
import           System.Directory           (doesFileExist, makeAbsolute)
import           System.Exit                (die)

import           Forge.Gitlab.Options
import           Forge.Gitlab.Types

decodeConfig :: FilePath -> IO (Either Y.ParseException GitlabConfig)
decodeConfig p = Y.decodeFileEither p

readConfig :: FilePath -> IO (GitlabConfig)
readConfig p = do
  exists <- doesFileExist p
  when (not exists) (die "Config file does not exist.")
  c <- decodeConfig p
  case c of
    Left err -> die (show err)
    Right c  -> return c

entrypoint :: GitlabOpts -> IO ()
entrypoint (GitlabOpts config group project cmd) = do
  c' <- readConfig config
  let token = accessToken c'
  case cmd of
    CreateVariable e k v -> createVariable token group project e k v
    UpdateVariable e k v -> updateVariable token group project e k v
    DeleteVariable e k   -> deleteVariable token group project e k

createVariable :: AccessToken -> String -> String ->  Environment -> VarKey -> VarValue -> IO ()
createVariable t g p (Environment e) (VarKey k) (VarValue v) = do
  let url = Url $ "https://gitlab.com/api/v4/projects/" <> g <> "%2F" <> p <> "/variables/"
  let method = "POST"
  let pairs =
          [ ("key", C8.pack k)
          , ("value", C8.pack v)
          , ("environment_scope", C8.pack e)
          ]
  initialRequest <- mkInitRequest url t method
  ret <- execRequest initialRequest (Just pairs)
  print ret

updateVariable :: AccessToken -> String -> String -> Environment -> VarKey -> VarValue -> IO ()
updateVariable t g p (Environment e) (VarKey k) (VarValue v) = do
  let url = Url $ "https://gitlab.com/api/v4/projects/" <> g <> "%2F" <> p <> "/variables/" <> k
  let method = "PUT"
  let pairs =
          [ ("value", C8.pack v)
          , ("environment_scope", C8.pack e)
          ]
  initialRequest <- mkInitRequest url t method
  ret <- execRequest initialRequest (Just pairs)
  print ret

-- TODO: Add environment scope here once the API supports it
deleteVariable :: AccessToken -> String -> String -> Environment -> VarKey -> IO ()
deleteVariable t g p (Environment e) (VarKey k) = do
  let url = Url $ "https://gitlab.com/api/v4/projects/" <> g <> "%2F" <> p <> "/variables/" <> k
  let method = "DELETE"
  initialRequest <- mkInitRequest url t method
  ret <- execRequest initialRequest Nothing
  print ret

execRequest :: Request -> Maybe [(C8.ByteString, C8.ByteString)] -> IO (L8.ByteString)
execRequest r pairs = do
  manager <- newManager tlsManagerSettings
  response <- case pairs of
    Nothing -> try $ httpLbs r manager
    Just p  -> try $ httpLbs (urlEncodedBody p r) manager
  case response of
    Left e         -> die (show (e :: HttpException))
    Right response -> return $ getResponseBody response

mkInitRequest :: Url -> AccessToken -> String -> IO Request
mkInitRequest (Url url) (AccessToken t) m = do
  initialRequest <- parseRequest url
  let request = setRequestHeader "Content-Type" ["application/json"] $ setRequestHeader "PRIVATE-TOKEN" [C8.pack t] $ initialRequest { method = C8.pack m }
  return request


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.HTTP where

import           RIO

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple     (getResponseBody)

import qualified RIO.ByteString.Lazy     as BL
import           System.Exit             (die)


execRequest :: Request -> IO (BL.ByteString)
execRequest r = do
  manager <- newManager tlsManagerSettings
  response <- try $ httpLbs r manager
  case response of
    Left e    -> die (show (e :: HttpException))
    Right res -> return $ getResponseBody res

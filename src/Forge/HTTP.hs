{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.HTTP where

import           Control.Exception          (try)
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple        (getResponseBody, setRequestHeader,
                                             setRequestQueryString)
import           Network.HTTP.Types.Status  (statusCode)
import           System.Exit                (die)


execRequest :: Request -> IO (L8.ByteString)
execRequest r = do
  manager <- newManager tlsManagerSettings
  response <- try $ httpLbs r manager
  case response of
    Left e         -> die (show (e :: HttpException))
    Right response -> return $ getResponseBody response


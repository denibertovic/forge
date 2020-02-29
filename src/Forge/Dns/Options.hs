{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Forge.Dns.Options where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import RIO

type Env = [(String, String)]

data DnsCommand = DnsUpdate DnsProvider [Text] (Maybe Text)

data DnsOpts = DnsOpts {cmd :: DnsCommand}

data DnsProvider = DigitalOceanDns

readDnsProvider :: String -> Maybe DnsProvider
readDnsProvider "digital-ocean" = Just DigitalOceanDns
readDnsProvider _ = Nothing

cmdDnsUpdate :: Env -> Mod CommandFields DnsCommand
cmdDnsUpdate env = command "update" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Update dns entry on Provider DNS."
    options = DnsUpdate <$> providerOpt env <*> domainNamesOpt env <*> ipAddressOpt env

dnsCmds :: Env -> Parser DnsCommand
dnsCmds env = subparser (cmdDnsUpdate env)

dnsOpts :: Env -> Parser DnsOpts
dnsOpts env = DnsOpts <$> dnsCmds env

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

providerOpt :: Env -> Parser DnsProvider
providerOpt _ =
  option (maybeReader readDnsProvider)
    ( long "provider"
        <> short 'p'
        <> metavar "PROVIDER"
        <> help "DNS provider. Available options: digital-ocean"
    )

domainNamesOpt :: Env -> Parser [Text]
domainNamesOpt _ =
  many $ textOption
    ( long "domains"
        <> short 'd'
        <> metavar "DOMAIN_NAMES"
        <> help "Domain names to set. Example: www.example.com or foo.example.com"
    )

ipAddressOpt :: Env -> Parser (Maybe Text)
ipAddressOpt _ =
  optional $
    textOption
      ( long "ip"
          <> short 'a'
          <> metavar "IP_ADDRESS"
          <> help "IP Address to use. If not set use current IP."
      )

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Forge.Dns.Options where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import RIO

type Env = [(String, String)]

data DnsCommand = DnsUpdate Text (Maybe Text)

data DnsOpts = DnsOpts {cmd :: DnsCommand}

cmdDnsUpdate :: Env -> Mod CommandFields DnsCommand
cmdDnsUpdate env = command "update" infos
  where
    infos = info (options <**> helper) desc
    desc = progDesc "Update dns entry on DO DNS."
    options = DnsUpdate <$> domainNameOpt env <*> ipAddressOpt env

dnsCmds :: Env -> Parser DnsCommand
dnsCmds env = subparser (cmdDnsUpdate env)

dnsOpts :: Env -> Parser DnsOpts
dnsOpts env = DnsOpts <$> dnsCmds env

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

domainNameOpt :: Env -> Parser Text
domainNameOpt _ =
  textOption
    ( long "name"
        <> short 'n'
        <> metavar "DOMAIN_NAME"
        <> help "Domain name to set. Example: domain.com or www.domain.com"
    )

ipAddressOpt :: Env -> Parser (Maybe Text)
ipAddressOpt _ =
  optional $
    textOption
      ( long "ip"
          <> short 'a'
          <> metavar "IP_ADDRESS"
          <> help "IP Address to use"
      )

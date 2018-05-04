{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Forge.Lib where


import           Control.Exception          (try)
import           Control.Monad              (when)
import           Control.Monad              (join)
import           Data.Aeson                 (FromJSON, ToJSON, Value (..),
                                             parseJSON, toJSON, (.:), (.=))
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Semigroup             ((<>))
import           Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Yaml                  (decodeFileEither)
import qualified Data.Yaml                  as Y
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple        (getResponseBody, setRequestHeader)
import           Network.HTTP.Types.Status  (statusCode)
import           Options.Applicative
import           System.Directory           (doesFileExist, makeAbsolute)
import           System.Environment         (lookupEnv, setEnv)
import           System.Exit                (ExitCode (..), die, exitWith)
import           System.Posix.User          (getRealUserID)

import           Forge.Gitlab

data ForgeCommand = CreateVariable Environment VarKey VarValue
                  | UpdateVariable Environment VarKey VarValue
                  | DeleteVariable Environment VarKey

data ForgeOpts = ForgeOpts {
                   configFilePath :: FilePath
                 , group          :: String
                 , project        :: String
                 , debug          :: Bool
                 , cmd            :: ForgeCommand}

newtype Environment = Environment String deriving (Eq, Show)
newtype VarKey = VarKey String deriving (Eq, Show)
newtype VarValue = VarValue String deriving (Eq, Show)
newtype Url = Url String deriving (Eq, Show)

type Env = [(String, String)]

environ :: (HasValue f) => String -> Env -> Mod f String
environ k env = maybe idm value . join $ parse <$> lookup k env
  where
    parse = either (const Nothing) Just . Right

textOption :: Mod OptionFields String -> Parser String
textOption = option (eitherReader (Right))

groupOpt env = textOption
        ( long "group"
        <> environ "GITLAB_GROUP" env
        <> short 'g'
        <> metavar "GITLAB_GROUP"
        <> help "Gitlab Group." )

projectOpt env = textOption
        ( long "project"
        <> environ "GITLAB_PROJECT" env
        <> short 'p'
        <> metavar "GITLAB_PROJECT"
        <> help "Gitlab Project." )

configPathOpt = strOption
        ( long "config"
        <> short 'c'
        <> metavar "path"
        <> help "absolute path to the config file." )

debugOpt = switch
        ( long "debug"
        <> help "Debug mode. Verbose output." )

cmds = subparser (cmdCreateVariable <> cmdUpdateVariable <> cmdDeleteVariable)

forgeOpts :: Env -> Parser ForgeOpts
forgeOpts env = ForgeOpts <$> configPathOpt <*> (groupOpt env) <*> (projectOpt env) <*> debugOpt <*> cmds

entrypoint :: ForgeOpts -> IO ()
entrypoint (ForgeOpts config group project debug cmd) = do
  c' <- readConfig config
  let token = accessToken c'
  case cmd of
    CreateVariable e k v -> createVariable token group project e k v
    UpdateVariable e k v -> updateVariable token group project e k v
    DeleteVariable e k   -> deleteVariable token group project e k

readEnv :: String -> Maybe Environment
readEnv v = Just $ Environment v

readKey :: String -> Maybe VarKey
readKey v = Just $ VarKey v

readVal :: String -> Maybe VarValue
readVal v = Just $ VarValue v

envOpt = option (maybeReader readEnv) (
       long "environment"
       <> short 'e'
       <> metavar "NAME"
       <> help "Name of the environment")

keyOpt = option (maybeReader readKey)(
       long "key"
       <> short 'k'
       <> metavar "KEY"
       <> help "Name of the variable")

valueOpt = option (maybeReader readVal) (
       long "value"
       <> short 'v'
       <> metavar "VALUE"
       <> help "Value of the variable.")

cmdCreateVariable = command "create" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Create gitlab variable."
          options = CreateVariable <$> envOpt <*> keyOpt <*> valueOpt

cmdUpdateVariable = command "update" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Create gitlab variable."
          options = UpdateVariable <$> envOpt <*> keyOpt <*> valueOpt

cmdDeleteVariable = command "delete" infos
    where infos = info (options <**> helper) desc
          desc = progDesc "Delte gitlab variable."
          options = DeleteVariable <$> envOpt <*> keyOpt

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


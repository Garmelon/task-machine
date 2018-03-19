{-# LANGUAGE RecordWildCards #-}

module TaskMachine.Config
  ( Config(..)
  , defaultConfig
  , defaultConfigFilePaths
  , CPException(..)
  , loadConfig
  , saveConfig
  ) where

import           Control.Exception
import           Data.Either

import qualified Data.ConfigFile   as C

data Config = Config
  { cThemes :: [FilePath]
  , cTaskDB :: FilePath
  }

defaultConfig :: Config
defaultConfig = Config
  { cThemes = []
  , cTaskDB = "~/.taskmachine/tasks.db"
  }

defaultConfigFilePaths :: [FilePath]
defaultConfigFilePaths = ["tasks.conf", "~/.taskmachine/tasks.conf"]

newtype CPException = CPException C.CPErrorData
  deriving (Show)

instance Exception CPException

toCPException :: (C.CPErrorData, String) -> CPException
toCPException (errorData, _) = CPException errorData

loadConfig :: FilePath -> IO Config
loadConfig path = do
  mcp <- C.readfile C.emptyCP path
  case mcp of
    Left e -> throwIO $ toCPException e
    Right cp ->
      let myThemes = fromRight (cThemes defaultConfig) $ C.get cp "DEFAULT" "themes"
          myTaskDB = fromRight (cTaskDB defaultConfig) $ C.get cp "DEFAULT" "taskdb"
      in  return Config
            { cThemes = myThemes
            , cTaskDB = myTaskDB
            }

configToParser :: Config -> C.ConfigParser
configToParser Config{..} = fromEither $ do
  cp1 <- C.set C.emptyCP "DEFAULT" "themes" (show cThemes)
  C.set cp1 "DEFAULT" "taskdb" cTaskDB
  where
    fromEither (Left e)  = throw $ toCPException e
    fromEither (Right v) = v

saveConfig :: FilePath -> Config -> IO ()
saveConfig path = writeFile path . C.to_string . configToParser

{-# LANGUAGE RecordWildCards #-}

module TaskMachine.Config
  ( HomeDir
  , Config(..)
  , defaultConfig
  , defaultConfigFilePaths
  , loadConfig
  , saveConfig
  ) where

import           Data.Either

import qualified Data.ConfigFile as C

type HomeDir = FilePath

data Config = Config
  { cThemes :: [FilePath]
  , cTaskDB :: FilePath
  }

defaultConfig :: HomeDir -> Config
defaultConfig homedir = Config
  { cThemes = []
  , cTaskDB = homedir ++ "/.taskmachine/tasks.db"
  }

defaultConfigFilePaths :: HomeDir ->  [FilePath]
defaultConfigFilePaths homedir =
  [homedir ++ "/.taskmachine/tasks.conf", "tasks.conf"]

loadConfig :: HomeDir -> FilePath -> IO (Either C.CPErrorData Config)
loadConfig homedir path = do
  ecp <- C.readfile C.emptyCP path
  case ecp of
    Left (e, _) -> return $ Left e
    Right cp ->
      let config = defaultConfig homedir
          myThemes = fromRight (cThemes config) $ C.get cp "DEFAULT" "themes"
          myTaskDB = fromRight (cTaskDB config) $ C.get cp "DEFAULT" "taskdb"
      in  return $ Right Config
            { cThemes = myThemes
            , cTaskDB = myTaskDB
            }

configToParser :: Config -> C.ConfigParser
configToParser Config{..} = fromEither $ do
  cp1 <- C.set C.emptyCP "DEFAULT" "themes" (show cThemes)
  C.set cp1 "DEFAULT" "taskdb" cTaskDB
  where
    fromEither (Left _)  = undefined -- This should not be able to fail.
    fromEither (Right v) = v

saveConfig :: FilePath -> Config -> IO ()
saveConfig path = writeFile path . C.to_string . configToParser

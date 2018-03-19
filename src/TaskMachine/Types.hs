module TaskMachine.Types
  ( Config(..)
  , defaultConfig
  ) where

data Config = Config
  { cThemes :: [FilePath]
  , cTaskDB :: FilePath
  }

defaultConfig :: Config
defaultConfig = Config
  { cThemes = []
  , cTaskDB = "~/.taskmachine/tasks.db"
  }

-- | A way to store the 'Task's that preserves the original task order

module TaskMachine.TaskList
  ( LTask(..)
  , fromTasks
  , loadLTasks
  ) where

import qualified Data.Vector         as V
import           Text.Megaparsec

import           TaskMachine.Todotxt

data LTask = LTask
  { ltaskNumber :: Integer
  -- ^ Sort by this number to get the original order of the tasks
  , ltaskTask   :: Task
  -- ^ The 'Task' itself
  } deriving (Show)

fromTasks :: [Task] -> [LTask]
fromTasks = zipWith LTask [1..]

loadLTasks :: FilePath -> IO (Either String (V.Vector LTask))
loadLTasks file = do
  content <- readFile file
  case parseTasks file content of
    Right tasks     -> pure $ Right $ V.fromList $ fromTasks tasks
    Left parseError -> pure $ Left $ show parseError

-- | A way to store the 'Task's that preserves the original task order.

module TaskMachine.LTask
  ( LTask(..)
  , fromTasks
  , loadLTasks
  , saveLTasks
  ) where

import Data.List
import Data.Function

import qualified Data.Vector         as V

import           TaskMachine.Todotxt

-- | A "ListTask" for use in the task list
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
    Right taskList     -> pure $ Right $ V.fromList $ fromTasks taskList
    Left parseError -> pure $ Left $ show parseError

saveLTasks :: V.Vector LTask -> FilePath -> IO ()
saveLTasks ltasks file = do
  let taskList = map ltaskTask $ sortBy (compare `on` ltaskNumber) $ V.toList ltasks
      text = unlines $ map formatTask taskList
  writeFile file text

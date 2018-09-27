-- | A way to store the 'Task's that preserves the original task order.
--
-- A @LTask@ stores a number representing its original position in addition to the 'Task' itself.
-- To restore the original order, the @LTask@s are sorted by this number.
-- When sorting this way, any @LTask@s created using 'lTask' are appended at the end.
--
-- LTasks can be deleted from any part of the list, but only appended to the end.
--
-- LTasks from different 'fromTasks' calls should /not/ be mixed together.

module TaskMachine.LTask
  ( LTask
  , lTask
  , toTask
  , fromTasks
  , toTasks
  , modifyLTask
  , sortLTasks
  , loadLTasks
  , saveLTasks
  ) where

import           Data.Function
import           Data.List

import qualified Data.Vector      as V
import           Text.Megaparsec

import           TaskMachine.Task

data Position = Old Integer | New
  deriving (Eq, Show, Ord)

data LTask = LTask
  { lPosition :: Position
  , lRealTask :: Task
  } deriving (Show)

lTask :: Task -> LTask
lTask = LTask New

toTask :: LTask -> Task
toTask = lRealTask

fromTasks :: [Task] -> [LTask]
fromTasks = zipWith LTask (map Old [1..])

toTasks :: [LTask] -> [Task]
toTasks = map toTask . sortLTasks

modifyLTask :: (Task -> Task) -> LTask -> LTask
modifyLTask f (LTask pos task) = LTask pos (f task)

sortLTasks :: [LTask] -> [LTask]
sortLTasks = sortBy (compare `on` lPosition)

loadLTasks :: FilePath -> IO (Either String (V.Vector LTask))
loadLTasks file = do
  content <- readFile file
  case parse pTasks file content of
    Right taskList  -> pure $ Right $ V.fromList $ fromTasks taskList
    Left parseError -> pure $ Left $ parseErrorPretty parseError

saveLTasks :: FilePath -> V.Vector LTask -> IO ()
saveLTasks file ltasks = do
  let text = formatTasks $ toTasks $ V.toList ltasks
  writeFile file text

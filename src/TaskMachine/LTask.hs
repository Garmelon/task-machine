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

import           Control.Exception
import           Data.Function
import           Data.List
import           System.IO.Error

import qualified Data.Vector       as V
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

{- Loading -}

data ErrorAction
  = ErrorMessage String
  | IgnoreError
  deriving (Show)

loadErrorMessage :: IOError -> Maybe ErrorAction
loadErrorMessage e
  | isDoesNotExistError e = Just IgnoreError
  | otherwise             = Just $ ErrorMessage $ show e

loadLTasks :: FilePath -> IO (Either String (V.Vector LTask))
loadLTasks file = do
  content <- tryJust loadErrorMessage $ readFile file
  case parse pTasks file <$> content of
    Left IgnoreError        -> pure $ Right V.empty
    Left (ErrorMessage msg) -> pure $ Left msg
    Right (Left parseError) -> pure $ Left $ parseErrorPretty parseError
    Right (Right taskList)  -> pure $ Right $ V.fromList $ fromTasks taskList

{- Saving -}

saveErrorMessage :: IOError -> Maybe String
saveErrorMessage e = Just $ show e

saveLTasks :: FilePath -> V.Vector LTask -> IO (Either String ())
saveLTasks file ltasks = do
  let text = formatTasks $ toTasks $ V.toList ltasks
  result <- tryJust saveErrorMessage $ writeFile file text
  case result of
    Left ioErrorMessage -> pure $ Left ioErrorMessage
    Right _             -> pure $ Right ()

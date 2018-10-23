module TaskMachine.UI.TaskList
  ( TaskList
  , taskList
  , taskListElements
  , renderTaskList
  , updateTaskList
  , sortTaskList
  , selectedTask
  , appendTask
  , replaceTask
  , deleteTask
  , modifyAllTasks
  ) where

import           Data.Function
import           Data.List

import qualified Brick                   as B
import qualified Brick.Widgets.List      as B
import qualified Data.Vector             as V
import qualified Graphics.Vty            as VTY

import           TaskMachine.LTask
import           TaskMachine.Task
import           TaskMachine.UI.Task
import           TaskMachine.UI.TaskEdit

newtype TaskList n = TaskList (B.List n LTask)

taskList :: n -> V.Vector LTask -> TaskList n
taskList name tasks = TaskList $ B.list name tasks 1

taskListElements :: TaskList n -> V.Vector LTask
taskListElements (TaskList list) = B.listElements list

renderRow :: Maybe (B.Widget n) -> Bool -> LTask -> B.Widget n
renderRow (Just w) True _ = w
renderRow _ _ lt          = renderTask (toTask lt)

renderLast :: (Ord n, Show n) => B.Widget n -> Bool -> B.List n LTask -> B.Widget n
renderLast widget focus list =
  let listWithPlaceholder = focusOnLastTask $ appendTask' emptyTask list
  in  B.renderList (renderRow (Just widget)) focus listWithPlaceholder

renderTaskList :: (Ord n, Show n) => Maybe (TaskEdit n) -> Bool -> TaskList n -> B.Widget n
renderTaskList Nothing focus (TaskList list)
  | listSize list == 0 = renderLast (B.str "--- empty ---") focus list
  | otherwise          = B.renderList (renderRow Nothing) focus list
renderTaskList (Just te) focus (TaskList list) =
  case editState te of
    ExistingTask -> B.renderList (renderRow (Just teWidget)) focus list
    NewTask      -> renderLast teWidget focus list
  where
    teWidget = renderTaskEdit focus te

updateTaskList :: Ord n => VTY.Event -> TaskList n -> B.EventM n (TaskList n)
updateTaskList event (TaskList list) =
  TaskList <$> B.handleListEventVi B.handleListEvent event list

sortTaskList :: TaskList n -> TaskList n
sortTaskList (TaskList list) =
  let index = B.listSelected list
      tasks = V.toList $ B.listElements list
      sortedTasks = sortBy (compareTasks `on` toTask) tasks
      newVector = V.fromList sortedTasks
  in  TaskList $ B.listReplace newVector index list

selectedTask :: TaskList n -> Maybe Task
selectedTask (TaskList list) = toTask . snd <$> B.listSelectedElement list

appendTask' :: Task -> B.List n LTask -> B.List n LTask
appendTask' task list =
  let size = listSize list
      lt = lTask task
  in  focusOnLastTask $ B.listInsert size lt list

appendTask :: Task -> TaskList n -> TaskList n
appendTask task (TaskList list) = TaskList $ appendTask' task list

replaceTask :: Task -> TaskList n -> TaskList n
replaceTask task (TaskList list) = TaskList $ B.listModify replace list
  where
    replace :: LTask -> LTask
    replace = modifyLTask (const task)

deleteTask :: TaskList n -> TaskList n
deleteTask tl@(TaskList list) =
  case B.listSelected list of
    Nothing -> tl
    Just index
      | index == 0 -> TaskList $ B.listRemove index list
      | otherwise  -> TaskList $ B.listMoveBy 1 $ B.listRemove index list

modifyAllTasks :: (Task -> Task) -> TaskList n -> TaskList n
modifyAllTasks f (TaskList list) =
  let index = B.listSelected list
      vector = B.listElements list
      vector' = V.map (modifyLTask f) vector
  in  TaskList $ B.listReplace vector' index list

{- helper functions -}

listSize :: B.List n e -> Int
listSize list = V.length $ B.listElements list

focusOnLastTask :: B.List n e -> B.List n e
focusOnLastTask list = B.listMoveTo (listSize list - 1) list

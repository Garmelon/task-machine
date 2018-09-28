module TaskMachine.UI.Behaviors.TaskList
  ( taskListBehavior
  ) where

import qualified Brick                   as B
import qualified Brick.Widgets.Edit      as B
import qualified Graphics.Vty            as VTY

import           TaskMachine.Task
import           TaskMachine.UI.TaskList
import           TaskMachine.UI.Types

startEdit :: UIState -> UIState
startEdit s =
  case taskListSelectedElement (tasks s) of
    Nothing -> undefined -- TODO: Add popup notification
    Just t  ->
      let edit = B.editor RTaskEdit (Just 1) (formatTask t)
      in s{taskEdit=Just edit}

taskListBehavior :: UIState -> VTY.Event -> NewState
-- Mark/unmark a task as completed
taskListBehavior s (VTY.EvKey (VTY.KChar 'x') []) = undefined s
-- Delete tasks
taskListBehavior s (VTY.EvKey (VTY.KChar 'd') []) = undefined s
-- Start editing a new task
taskListBehavior s (VTY.EvKey (VTY.KChar 'e') []) = B.continue (startEdit s)
-- Update the task list (scroll etc.)
taskListBehavior s e = do
  newTasks <- updateTaskList e (tasks s)
  B.continue s{tasks=newTasks}

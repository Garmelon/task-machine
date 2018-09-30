module TaskMachine.UI.Behaviors
  ( taskListBehavior
  , taskEditBehavior
  ) where

import qualified Brick                   as B
import qualified Brick.Widgets.Edit      as B
import           Control.Monad.Trans
import qualified Data.Text.Zipper        as T
import qualified Graphics.Vty            as VTY
import           Text.Megaparsec

import           TaskMachine.Task
import           TaskMachine.UI.TaskList
import           TaskMachine.UI.Types
import TaskMachine.UI.Stuff

startEdit :: UIState -> UIState
startEdit s =
  case taskListSelectedElement (tasks s) of
    Nothing -> undefined -- TODO: Add popup notification
    Just t  ->
      let edit = B.editor RTaskEdit (Just 1) (formatTask t)
      in s{taskEdit=Just edit}

finishEdit :: B.Editor String RName -> UIState -> UIState
finishEdit edit s =
  let editedText = unlines $ B.getEditContents edit
  in  case parse pTask "edited task" editedText of
    Left parseError -> undefined parseError -- TODO: Add popup notification
    Right newTask   ->
      let newTaskList = taskListModify (const newTask) (tasks s)
      in  s{tasks=newTaskList, taskEdit=Nothing}

taskEditBehavior :: B.Editor String RName -> UIState -> VTY.Event -> NewState
taskEditBehavior _    s (VTY.EvKey VTY.KEsc   []) = B.continue s{taskEdit=Nothing}
taskEditBehavior edit s (VTY.EvKey VTY.KHome  []) = B.continue s{taskEdit=Just (B.applyEdit T.gotoBOL edit)}
taskEditBehavior edit s (VTY.EvKey VTY.KEnd   []) = B.continue s{taskEdit=Just (B.applyEdit T.gotoEOL edit)}
taskEditBehavior edit s (VTY.EvKey VTY.KEnter []) = do
  newState <- liftIO $ saveTasks $ finishEdit edit s
  B.continue newState
taskEditBehavior edit s e = do
  newEdit <- B.handleEditorEvent e edit
  B.continue s{taskEdit=Just newEdit}

taskListBehavior :: UIState -> VTY.Event -> NewState
-- Reload while running
taskListBehavior s (VTY.EvKey (VTY.KChar 'r') []) = actionLoad s
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

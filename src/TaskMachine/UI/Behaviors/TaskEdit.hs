module TaskMachine.UI.Behaviors.TaskEdit
  ( taskEditBehavior
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
  let newState = finishEdit edit s
  liftIO $ saveUIState newState
  B.continue newState
taskEditBehavior edit s e = do
  newEdit <- B.handleEditorEvent e edit
  B.continue s{taskEdit=Just newEdit}

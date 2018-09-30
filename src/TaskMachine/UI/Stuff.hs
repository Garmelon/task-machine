module TaskMachine.UI.Stuff where

import qualified Brick                   as B
import           Control.Monad.Trans

import           TaskMachine.LTask
import           TaskMachine.Options
import           TaskMachine.UI.Popup
import           TaskMachine.UI.TaskList
import           TaskMachine.UI.Types

actionQuit :: UIState -> NewState
actionQuit = B.halt

actionDoNothing :: UIState -> NewState
actionDoNothing = B.continue

actionLoad :: UIState -> NewState
actionLoad s = do
  state <- liftIO $ loadTasks s
  B.continue state

loadTasks :: UIState -> IO UIState
loadTasks s = do
  let file = oTodofile $ options s
  result <- loadLTasks file
  case result of
    Right ltasks      -> pure s{tasks=taskList RTaskList ltasks}
    Left errorMessage ->
      let p = popup "Error loading tasks" errorMessage
                [ ("Retry", actionLoad)
                , ("Quit", actionQuit)
                ]
      in  pure s{errorPopup=Just p}

actionSave :: UIState -> NewState
actionSave s = do
  state <- liftIO $ saveTasks s
  B.continue state

saveTasks :: UIState -> IO UIState
saveTasks s = do
  let filepath = oTodofile (options s)
      ltasks = taskListElements (tasks s)
  result <- saveLTasks filepath ltasks
  case result of
    Right _ -> pure s
    Left errorMessage ->
      let p = popup "Error saving tasks" errorMessage
                [ ("Retry", actionSave)
                , ("Continue without saving", actionDoNothing)
                , ("Quit", actionQuit)
                ]
      in  pure s{errorPopup=Just p}

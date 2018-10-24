module TaskMachine.UI.Behaviors
  ( Behavior
  --, emptyBehavior
  -- * Miscellaneous
  , getCurrentDay
  , closeModifier
  -- * Behaviors
  , popupBehavior
  , taskListBehavior
  , taskEditBehavior
  -- * Actions
  , actionLoad
  , actionSave
  , actionDelete
  , actionEditNew
  , actionEditSelected
  , actionToggleCompletion
  , actionSortTasks
  , actionFinishEdit
  ) where

import           Control.Monad

import qualified Brick                   as B
--import qualified Brick.Widgets.Edit      as B
import           Control.Monad.Trans
--import qualified Data.Text.Zipper        as T
import qualified Graphics.Vty            as VTY
--import           Text.Megaparsec
import           Data.Time

import           TaskMachine.LTask
import           TaskMachine.Options
import           TaskMachine.Task
import           TaskMachine.UI.Popup
import           TaskMachine.UI.TaskEdit
import           TaskMachine.UI.TaskList
import           TaskMachine.UI.Types

type Behavior = UIState -> VTY.Event -> B.EventM RName (B.Next UIState)

type Action = UIState -> B.EventM RName UIState

{- Miscellaneous -}

getCurrentDay :: IO Day
getCurrentDay = utctDay <$> liftIO getCurrentTime

closeModifier :: Behavior -> Behavior
closeModifier _ s (VTY.EvKey VTY.KEsc        []) = B.halt s
closeModifier _ s (VTY.EvKey (VTY.KChar 'q') []) = B.halt s
closeModifier f s e                              = f s e -- wrapper around another behavior

{- Popups -}

popupBehavior :: Popup RName (UIState -> NewState) -> Behavior
popupBehavior p s (VTY.EvKey VTY.KEnter []) =
  case popupSelection p of
    Nothing     -> B.continue s{errorPopup=Nothing} -- Just close, no action was specified
    Just action -> action s{errorPopup=Nothing} -- Do the thing! (and close the popup)
popupBehavior p s e = do
  newPopup <- handlePopupEvent e p
  B.continue s{errorPopup=Just newPopup}

{- On the task list -}

-- (re-)loading

actionLoad :: Action
actionLoad s = do
  let file = oTodofile $ options s
  result <- liftIO $ loadLTasks file
  case result of
    Right ltasks      -> pure s{tasks=taskList RTaskList ltasks}
    Left errorMessage ->
      let p = popup "Error loading tasks" errorMessage
                [ ("Retry", actionLoad >=> B.continue)
                , ("Quit", B.halt)
                ]
      in  pure s{errorPopup=Just p}

-- saving

actionSave :: Action
actionSave s = do
  let filepath = oTodofile (options s)
      ltasks = taskListElements (tasks s)
  result <- liftIO $ saveLTasks filepath ltasks
  case result of
    Right _ -> pure s
    Left errorMessage ->
      let p = popup "Error saving tasks" errorMessage
                [ ("Retry", actionSave >=> B.continue)
                , ("Continue without saving", B.continue)
                , ("Quit", B.halt)
                ]
      in  pure s{errorPopup=Just p}

-- deleting a task

actionDelete :: Action
actionDelete s = pure s{tasks=deleteTask (tasks s)}

-- beginning an edit

actionEditNew :: Action
actionEditNew s = do
  today <- liftIO getCurrentDay
  let task = newTask today
      edit = taskEdit RTaskEdit task NewTask
  pure s{editor=Just edit}

actionEditSelected :: Action
actionEditSelected s =
  case selectedTask (tasks s) of
    Nothing -> pure s
    Just t  ->
      let edit = taskEdit RTaskEdit t ExistingTask
      in pure s{editor=Just edit}

-- toggling completion

actionToggleCompletion :: Action
actionToggleCompletion s =
  case selectedTask (tasks s) of
    Nothing -> pure s
    Just task -> do
      newCompletion <- case taskCompletion task of
        Complete _ -> pure Incomplete
        Incomplete -> Complete . Just <$> liftIO getCurrentDay
      let task' = task{taskCompletion=newCompletion}
          newTaskList = replaceTask task' (tasks s)
      pure s{tasks=newTaskList}

-- sorting

actionSortTasks :: Action
actionSortTasks s = pure s{tasks=sortTaskList (tasks s)}

-- cleaning up tasks

cleanUpTask :: Day -> Task -> Task
cleanUpTask today (Task (Complete Nothing) p d Nothing desc) =
  Task (Complete (Just today)) p d (Just today) desc
cleanUpTask today (Task (Complete Nothing) p d c desc) =
  Task (Complete (Just today)) p d c desc
cleanUpTask today (Task c p d Nothing desc) =
  Task c p d (Just today) desc
cleanUpTask _ t = t

actionCleanUp :: Action
actionCleanUp s = do
  today <- liftIO getCurrentDay
  let tasks' = modifyAllTasks (cleanUpTask today) (tasks s)
  pure s{tasks=tasks'}

-- combining all of the above...

taskListBehavior :: Behavior
-- Clean up: Add todays date where creation/completion date is missing
taskListBehavior s (VTY.EvKey (VTY.KChar 'c') []) =
  actionCleanUp >=> actionSave >=> B.continue $ s
-- Delete currently selected task (implicit save)
taskListBehavior s (VTY.EvKey (VTY.KChar 'd') []) =
  actionDelete >=> actionSave >=> B.continue $ s
-- Begin editing currently selected task
taskListBehavior s (VTY.EvKey (VTY.KChar 'e') []) =
  actionEditSelected >=> B.continue $ s
-- Begin creating new task
taskListBehavior s (VTY.EvKey (VTY.KChar 'n') []) =
  actionEditNew >=> B.continue $ s
-- Reload tasks (and sort them)
taskListBehavior s (VTY.EvKey (VTY.KChar 'r') []) =
  actionLoad >=> B.continue $ s
-- Sort tasks
taskListBehavior s (VTY.EvKey (VTY.KChar 's') []) =
  actionSortTasks >=> B.continue $ s
-- Toggle completion (implicit save)
taskListBehavior s (VTY.EvKey (VTY.KChar 'x') []) =
  actionToggleCompletion >=> actionSave >=> B.continue $ s
-- Update the task list (scroll etc.)
taskListBehavior s e = do
  newTasks <- updateTaskList e (tasks s)
  B.continue s{tasks=newTasks}

{- In the task editor -}

actionFinishEdit :: TaskEdit RName -> Action
actionFinishEdit t = pure . finishEdit t

-- get result of task editing
-- if editing an existing task, modify that task
-- if editing a new task, append that task
finishEdit :: TaskEdit RName -> UIState -> UIState
finishEdit edit s =
  case editedTask edit of
    Left e     ->
      let p = popup "Syntax error" e
                [ ("Continue editing", B.continue)
                , ("Abort", \s' -> B.continue s'{editor=Nothing})
                ]
      in  s{errorPopup=Just p}
    Right task -> s{tasks=modify task, editor=Nothing}
  where
    modify :: Task -> TaskList RName
    modify task = case editState edit of
      ExistingTask -> replaceTask task $ tasks s
      NewTask      -> appendTask task $ tasks s

taskEditBehavior :: TaskEdit RName  -> Behavior
taskEditBehavior _    s (VTY.EvKey VTY.KEsc []) = B.continue s{editor=Nothing}
taskEditBehavior edit s (VTY.EvKey VTY.KEnter []) =
  actionFinishEdit edit >=> actionSave >=> B.continue $ s
--taskEditBehavior edit s (VTY.EvKey VTY.KEnter []) = do
--  newState <- liftIO $ saveTasks $ finishEdit edit s
--  B.continue newState
taskEditBehavior edit s e = do
  newEdit <- updateTaskEdit e edit
  B.continue s{editor=Just newEdit}

{-
-- Reload while running
taskListBehavior s (VTY.EvKey (VTY.KChar 'r') []) = actionLoad s
-- Mark/unmark a task as completed
taskListBehavior s (VTY.EvKey (VTY.KChar 'x') []) = undefined s
-- Delete tasks
taskListBehavior s (VTY.EvKey (VTY.KChar 'd') []) = undefined s
-- Delete tasks
taskListBehavior s (VTY.EvKey (VTY.KChar 'd') []) = undefined s
-- Start editing a new task
taskListBehavior s (VTY.EvKey (VTY.KChar 'e') []) = B.continue (startEdit s)
-- Update the task list (scroll etc.)
taskListBehavior s e = do
  newTasks <- updateTaskList e (tasks s)
  B.continue s{tasks=newTasks}
-}

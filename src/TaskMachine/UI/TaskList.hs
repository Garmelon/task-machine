module TaskMachine.UI.TaskList
  ( TaskList
  , taskList
  , renderTaskList
  , taskListElements
  , taskListFilter
  , taskListSelectedElement
  , taskListModify
  ) where

--import           Data.Void

import qualified Brick               as B
import qualified Brick.Widgets.List  as B
import qualified Data.Vector         as V
--import qualified Brick.Focus          as B
--import qualified Brick.Widgets.Edit   as B
--import qualified Data.Text.Zipper     as T
--import qualified Graphics.Vty         as VTY
--import           Text.Megaparsec

import           TaskMachine.LTask
import           TaskMachine.Task
import           TaskMachine.UI.Task
--import           TaskMachine.Options
--import           TaskMachine.UI.Popup
--import           TaskMachine.UI.Types

data TaskList n = TaskList
  { visibleTasks   :: B.List n LTask
  , invisibleTasks :: V.Vector LTask
  } deriving (Show)

newList :: n -> V.Vector LTask -> B.List n LTask
newList name ltasks = B.list name ltasks 1

taskList :: n -> V.Vector LTask -> TaskList n
taskList name ltasks = TaskList{visibleTasks=newList name ltasks, invisibleTasks=V.empty}

-- TODO: render while editing
renderTaskList :: (Ord n, Show n) => Bool -> TaskList n -> B.Widget n
renderTaskList focus tl = B.renderList (const $ renderTask . toTask) focus (visibleTasks tl)

{- Managing tasks -}

taskListElements :: TaskList n -> V.Vector LTask
taskListElements tl = B.listElements (visibleTasks tl) <> invisibleTasks tl

taskListFilter :: (Task -> Bool) -> TaskList n -> TaskList n
taskListFilter f tl =
  let (yes, no) = V.partition (f . toTask) $ taskListElements tl
      name = B.listName (visibleTasks tl)
      list = newList name yes
  in  TaskList{visibleTasks=list, invisibleTasks=no}

taskListSelectedElement :: TaskList n -> Maybe Task
taskListSelectedElement tl = toTask . snd <$> B.listSelectedElement (visibleTasks tl)

taskListModify :: (Task -> Task) -> TaskList n -> TaskList n
taskListModify f tl =
  let list = B.listModify (modifyLTask f) (visibleTasks tl)
  in  tl{visibleTasks=list}

{-
{- Managing the tasks -}

allTasks :: UIState -> V.Vector LTask
allTasks s =
  let visible   = B.listElements $ taskList s
      invisible = invisibleTasks s
  in  visible <> invisible

newTaskList :: V.Vector LTask -> B.List RName LTask
newTaskList ltasks = B.list RTaskList ltasks 1

-- TODO: Catch errors when loading tasks
loadTasks :: UIState -> IO UIState
loadTasks s = do
  let file = oTodofile $ options s
  result <- loadLTasks file
  case result of
    Left errorMessage -> pure s{errorPopup=Just $ popupOk "Error loading tasks" errorMessage}
    Right ltasks      -> pure s{taskList=newTaskList ltasks, invisibleTasks=V.empty}

-- TODO: Catch errors when saving tasks
saveTasks :: UIState -> IO UIState
saveTasks s = do
  let file = oTodofile $ options s
      ltasks = allTasks s
  saveLTasks file ltasks
  pure s

filterTasks :: (Task -> Bool) -> UIState -> UIState
filterTasks f s =
  let (yes, no) = V.partition (f . toTask) (allTasks s)
  in  s{taskList=newTaskList yes, invisibleTasks=no}

{- Rendering -}

renderLTask :: Maybe (B.Editor String RName) -> Bool -> LTask -> B.Widget RName
renderLTask _           False ltask = renderTask $ toTask ltask
renderLTask Nothing     True  ltask = renderTask $ toTask ltask
renderLTask _           _     _     = undefined
--renderLTask (Just edit) True  _     = B.renderEditor (B.str . unlines) True edit

renderTaskList :: UIState -> B.Widget RName
renderTaskList s =
  let inFocus = B.focusGetCurrent (focus s) == Just BRTaskList
  in  B.renderList (renderLTask Nothing) inFocus (taskList s)

{- Updating state -}

taskListBehavior :: UIState -> VTY.Event -> NewState
taskListBehavior = undefined

updateTaskList :: UIState -> B.BrickEvent RName () -> B.EventM RName (B.Next UIState)
updateTaskList = undefined
-}

{-
widgetPriority :: B.AttrName -> Maybe Priority -> B.Widget n
widgetPriority _         Nothing     = B.emptyWidget
widgetPriority highlight (Just prio) =
  let attrName = highlight <> "priority" <> B.attrName [priorityToChar prio]
      text = formatPriority prio ++ " "
  in  B.withAttr attrName $ B.str text

widgetDescription :: B.AttrName -> String -> B.Widget n
widgetDescription highlight desc =
  let attrName = highlight <> "description"
  in  B.withAttr attrName $ B.str desc

renderLTask :: Bool -> LTask -> B.Widget RName
renderLTask highlight (LTask _ Task{..}) =
  let attrHighlight = if highlight then "highlight" else "normal"
      wCompleted   = B.str $ if taskCompleted then "x " else "  "
      wPriority    = widgetPriority    attrHighlight taskPriority
      wDescription = widgetDescription attrHighlight taskDescription
  in  B.hBox [wCompleted, wPriority, wDescription]
-}

--type Editor = B.Editor String RName
--type TaskList = B.List RName LTask

{- Editing tasks -}

{-
toEditText :: Task -> String
toEditText Task{taskPriority=Nothing, taskDescription=d} = descriptionToString d
toEditText Task{taskPriority=Just p, taskDescription=d} = formatPriority p ++ " " ++ descriptionToString d

pEditText :: Parser (Maybe Priority, String)
pEditText = undefined
--pEditText = do
--  prio <- maybeParse (andSpace pPriority)
--  notFollowedBy (andSpace pDates)
--  desc <- untilEndOfLine
--  pure (prio, desc)

parseEditText :: String -> Either (ParseError Char Void) (Maybe Priority, String)
parseEditText = parse pEditText "edited task"
-}

{- Updating state -}

{-
startEdit :: UIState -> UIState
startEdit s =
  case B.listSelectedElement (taskList s) of
    Nothing -> s
    Just (_, LTask _ t) ->
      let edit = B.editor RTaskEdit (Just 1) (toEditText t)
      in  s{taskEdit=Just edit}

finishEdit :: UIState -> UIState
finishEdit s@UIState{taskEdit=Just edit} =
  case B.getEditContents edit of
    [line] -> case parseEditText line of
      Right (prio, desc) ->
        --let changeTask (LTask n t) = LTask n t{taskPriority=prio, taskDescription=desc}
        let changeTask (LTask n t) = LTask n t{taskPriority=prio, taskDescription=undefined desc}
            newList = B.listModify changeTask (taskList s)
        in  s{taskList=newList, taskEdit=Nothing}

      Left parseError -> s{errorPopup=Just $ popup "Parse error" (parseErrorTextPretty parseError)}
    _ -> s{errorPopup=Just $ popup "Empty editor" "Enter a task description."}
finishEdit s = s

updateEditor :: B.Editor String RName -> VTY.Event -> B.EventM RName (B.Editor String RName)
updateEditor edit (VTY.EvKey VTY.KHome []) = pure $ B.applyEdit T.gotoBOL edit
updateEditor edit (VTY.EvKey VTY.KEnd  []) = pure $ B.applyEdit T.gotoEOL edit
updateEditor edit e                        = B.handleEditorEvent e edit

updateTaskList :: UIState -> B.BrickEvent RName () -> B.EventM RName (B.Next UIState)
-- Exit application
updateTaskList s@UIState{taskEdit=Nothing} (B.VtyEvent (VTY.EvKey VTY.KEsc [])) = B.halt s

-- Test stuff
updateTaskList s e = do
  let changeTask (LTask n t) = LTask n t{taskDescription=show e}
      newList = B.listModify changeTask (taskList s)
  B.continue s{taskList=newList}

-- Scroll focus
updateTaskList s (B.VtyEvent (VTY.EvKey VTY.KBackTab     [])) = B.continue $ bigFocusPrev s
updateTaskList s (B.VtyEvent (VTY.EvKey (VTY.KChar '\t') [])) = B.continue $ bigFocusNext s
-- Start editing the current task
updateTaskList s@UIState{taskEdit=Nothing} (B.VtyEvent (VTY.EvKey (VTY.KChar 'e') [])) = B.continue $ startEdit s
-- Update the task list
updateTaskList s@UIState{taskEdit=Nothing} (B.VtyEvent e) = do
  newList <- B.handleListEventVi B.handleListEvent e (taskList s)
  B.continue s{taskList=newList}
-- Exit the editor (losing all changes)
updateTaskList s@UIState{taskEdit=Just _} (B.VtyEvent (VTY.EvKey VTY.KEsc [])) = B.continue $ s{taskEdit=Nothing}
-- Exit the editor (keeping all changes)
updateTaskList s@UIState{taskEdit=Just _} (B.VtyEvent (VTY.EvKey VTY.KEnter [])) = B.continue $ finishEdit s
-- Update the editor
updateTaskList s@UIState{taskEdit=Just edit} (B.VtyEvent e) = do
  newTaskEdit <- updateEditor edit e
  B.continue s{taskEdit=Just newTaskEdit}
-- Catch everything else
updateTaskList s _ = B.halt s
--updateTaskList list (Just editor) (B.VtyEvent e) = (,) <$> const list <*> B.handleEditorEvent e editor
-}

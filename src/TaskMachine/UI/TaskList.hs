{-# LANGUAGE OverloadedStrings #-}

module TaskMachine.UI.TaskList where

import           Data.Void

import qualified Brick                as B
import qualified Brick.Focus          as B
import qualified Brick.Widgets.Edit   as B
import qualified Brick.Widgets.List   as B
import qualified Data.Text.Zipper     as T
import qualified Graphics.Vty         as VTY
import           Text.Megaparsec

import           TaskMachine.LTask
import           TaskMachine.Todotxt
import           TaskMachine.UI.Types

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

{- Rendering -}

renderLTask :: Maybe (B.Editor String RName) -> Bool -> LTask -> B.Widget RName
renderLTask _           False (LTask _ t) = B.str $ formatTask t
renderLTask Nothing     True  (LTask _ t) = B.str $ formatTask t
renderLTask (Just edit) True  _           = B.renderEditor (B.str . unlines) True edit

renderTaskList :: UIState -> B.Widget RName
renderTaskList s =
  let inFocus = B.focusGetCurrent (focus s) == Just BRTaskList
  in  B.renderList (renderLTask (taskEdit s)) inFocus (taskList s)

{- Editing tasks -}

toEditText :: Task -> String
toEditText Task{taskPriority=Nothing, taskDescription=d} = d
toEditText Task{taskPriority=Just p, taskDescription=d} = formatPriority p ++ " " ++ d

pEditText :: Parser (Maybe Priority, String)
pEditText = (,) <$> maybeParse (andSpace pPriority) <*> untilEndOfLine

parseEditText :: String -> Either (ParseError Char Void) (Maybe Priority, String)
parseEditText = parse pEditText "edited task"

{- Updating state -}

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
        let changeTask (LTask n t) = LTask n t{taskPriority=prio, taskDescription=desc}
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

{-
-- Test stuff
updateTaskList s e = do
  let changeTask (LTask n t) = LTask n t{taskDescription=show e}
      newList = B.listModify changeTask (taskList s)
  B.continue s{taskList=newList}
-}

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

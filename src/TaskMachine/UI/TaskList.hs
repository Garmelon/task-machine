{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TaskMachine.UI.TaskList where

import qualified Brick                as B
import qualified Brick.Widgets.Edit   as B
import qualified Brick.Widgets.List   as B
import qualified Graphics.Vty as VTY

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
renderLTask _           False (LTask _ t) = B.withAttr "normal"    $ B.str $ formatTask t
renderLTask Nothing     True  (LTask _ t) = B.withAttr "highlight" $ B.str $ formatTask t
renderLTask (Just edit) True  _           = B.withAttr "highlight" $ B.renderEditor (B.str . unlines) True edit

renderTaskList :: B.List RName LTask -> Maybe (B.Editor String RName) -> Bool -> B.Widget RName
renderTaskList taskList edit focus = B.renderList (renderLTask edit) focus taskList

{- Updating state -}

updateTaskList :: UIState -> B.BrickEvent RName () -> B.EventM RName (B.Next UIState)
-- Exit application
updateTaskList s@UIState{taskEdit=Nothing} (B.VtyEvent (VTY.EvKey VTY.KEsc        [])) = B.halt s
-- Scroll focus
updateTaskList s (B.VtyEvent (VTY.EvKey VTY.KBackTab     [])) = B.continue $ bigFocusPrev s
updateTaskList s (B.VtyEvent (VTY.EvKey (VTY.KChar '\t') [])) = B.continue $ bigFocusNext s
-- Start editing the current task
updateTaskList s@UIState{taskEdit=Nothing} (B.VtyEvent (VTY.EvKey (VTY.KChar 'e') [])) =
  case B.listSelectedElement (taskList s) of
    Nothing               -> B.continue s
    Just (_, (LTask _ t)) ->
      let edit = B.editor RTaskEdit (Just 1) ("- editor test -" ++ formatTask t)
      in  B.continue s{taskEdit=Just edit}
-- Update the task list
updateTaskList s@UIState{taskEdit=Nothing} (B.VtyEvent e) = do
  newList <- B.handleListEventVi B.handleListEvent e (taskList s)
  B.continue s{taskList=newList}
-- Exit the editor (losing all changes)
updateTaskList s@UIState{taskEdit=Just _} (B.VtyEvent (VTY.EvKey VTY.KEsc [])) = B.continue $ s{taskEdit=Nothing}
-- Exit the editor (keeping all changes)
updateTaskList s@UIState{taskEdit=Just _} (B.VtyEvent (VTY.EvKey VTY.KEnter [])) = do
  let changeTask (LTask n t) = LTask n t{taskDescription="hehe, changed"}
      newList = B.listModify changeTask (taskList s)
  B.continue s{taskList=newList, taskEdit=Nothing}
-- Update the editor
updateTaskList s@UIState{taskEdit=Just edit} (B.VtyEvent e) = do
  newEdit <- B.handleEditorEvent e edit
  B.continue s{taskEdit=Just newEdit}
-- Catch everything else
updateTaskList s _ = B.halt s
--updateTaskList list (Just editor) (B.VtyEvent e) = (,) <$> const list <*> B.handleEditorEvent e editor

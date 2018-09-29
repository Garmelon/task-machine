module TaskMachine.UI.TaskList
  ( TaskList
  , taskList
  , renderTaskList
  , updateTaskList
  , taskListElements
  , taskListFilter
  , taskListSelectedElement
  , taskListModify
  ) where

import qualified Brick               as B
import qualified Brick.Widgets.Edit  as B
import qualified Brick.Widgets.List  as B
import qualified Data.Vector         as V
import qualified Graphics.Vty        as VTY

import           TaskMachine.LTask
import           TaskMachine.Task
import           TaskMachine.UI.Task

data TaskList n = TaskList
  { visibleTasks   :: B.List n LTask
  , invisibleTasks :: V.Vector LTask
  } deriving (Show)

newList :: n -> V.Vector LTask -> B.List n LTask
newList name ltasks = B.list name ltasks 1

taskList :: n -> V.Vector LTask -> TaskList n
taskList name ltasks = TaskList{visibleTasks=newList name ltasks, invisibleTasks=V.empty}

renderLTask :: (Ord n, Show n) => Maybe (B.Editor String n) -> Bool -> LTask -> B.Widget n
renderLTask (Just e) True _ = B.renderEditor (B.str . unlines) True e
renderLTask _ _ lt          = renderTask (toTask lt)

renderTaskList :: (Ord n, Show n) => Maybe (B.Editor String n) -> Bool -> TaskList n -> B.Widget n
renderTaskList edit focus tl  = B.renderList (renderLTask edit) focus (visibleTasks tl)

updateTaskList :: (Ord n) => VTY.Event -> TaskList n -> B.EventM n (TaskList n)
updateTaskList e tl = do
  updatedList <- B.handleListEventVi B.handleListEvent e (visibleTasks tl)
  pure tl{visibleTasks=updatedList}

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

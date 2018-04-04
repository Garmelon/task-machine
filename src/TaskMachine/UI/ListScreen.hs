module TaskMachine.UI.ListScreen where
{-
  ( ListScreen
  , newListScreen
  , renderListScreen
  , updateListScreen
  ) where

import qualified Brick                  as B
import qualified Brick.Widgets.List     as B
import           Data.Time
import qualified Graphics.Vty.Input.Events as VTY
import qualified Data.Vector            as V
import qualified Database.SQLite.Simple as DB

import qualified TaskMachine.Database   as TM
import qualified TaskMachine.Task       as TM
import qualified TaskMachine.UI.Types   as TM

type Res = TM.ResourceName

newtype ListScreen = ListScreen (B.List Res TM.Task)

newListScreen :: DB.Connection -> IO ListScreen
newListScreen conn = do
  today <- utctDay <$> getCurrentTime
  relevant <- map TM.fromTaskRow <$> TM.selectRelevantTasks conn today
  let sorted = relevant -- TM.sort??? relevant
      vector = V.fromList sorted
      list   = B.list TM.RTaskList vector 1
  return $ ListScreen list

renderTask :: Bool -> TM.Task -> B.Widget Res
renderTask _ task = B.txt $ TM.taskDescription task

renderListScreen :: Bool -> ListScreen -> B.Widget Res
renderListScreen focused (ListScreen list) = B.renderList renderTask focused list

updateListScreen :: VTY.Event -> ListScreen -> B.EventM Res ListScreen
updateListScreen event (ListScreen list) =
  ListScreen <$> B.handleListEventVi B.handleListEvent event list
-}

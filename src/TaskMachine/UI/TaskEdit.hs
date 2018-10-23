module TaskMachine.UI.TaskEdit
  ( TaskEdit
  , EditState(..)
  , taskEdit
  , editState
  , editedTask
  , renderTaskEdit
  , updateTaskEdit
  ) where

import qualified Brick              as B
import qualified Brick.Widgets.Edit as B
import qualified Data.Text.Zipper   as T
import qualified Graphics.Vty       as VTY
import           Text.Megaparsec

import           TaskMachine.Task

data TaskEdit n = TaskEdit EditState (B.Editor String n)
  deriving (Show)

data EditState = ExistingTask | NewTask
  deriving (Show)

taskEdit :: n -> Task -> EditState -> TaskEdit n
taskEdit name task s = TaskEdit s $ B.editor name (Just 1) (formatTask task)

editState :: TaskEdit n -> EditState
editState (TaskEdit s _) = s

editedLine :: TaskEdit n -> Either String String
editedLine (TaskEdit _ edit) =
  case B.getEditContents edit of
    [s] -> Right s
    _   -> Left "Editor empty"

editedTask :: TaskEdit n -> Either String Task
editedTask te = do
  s <- editedLine te
  case parse pTask "task editor" s of
    Left parseError -> Left $ parseErrorPretty parseError
    Right task      -> Right task

renderRows :: [String] -> B.Widget n
renderRows = B.vBox . map B.str

renderTaskEdit :: (Ord n, Show n) => Bool -> TaskEdit n -> B.Widget n
renderTaskEdit focus (TaskEdit _ edit) = B.renderEditor renderRows focus edit

updateTaskEdit :: Ord n => VTY.Event -> TaskEdit n -> B.EventM n (TaskEdit n)
updateTaskEdit (VTY.EvKey VTY.KHome []) (TaskEdit s edit) =
  pure $ TaskEdit s $ B.applyEdit T.gotoBOL edit
updateTaskEdit (VTY.EvKey VTY.KEnd []) (TaskEdit s edit) =
  pure $ TaskEdit s $ B.applyEdit T.gotoEOL edit
updateTaskEdit event (TaskEdit s edit) =
  TaskEdit s <$> B.handleEditorEvent event edit

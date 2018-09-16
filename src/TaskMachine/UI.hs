module TaskMachine.UI where

--import           Data.Monoid
--
import qualified Brick                     as B
import qualified Brick.Focus               as B
import qualified Brick.Themes              as B
import qualified Graphics.Vty.Input.Events as VTY

import           TaskMachine.UI.NewTask
import           TaskMachine.UI.TaskList
import           TaskMachine.UI.TopBar
import           TaskMachine.UI.Types
--import qualified Database.SQLite.Simple    as DB
--import qualified Brick.Themes         as B
--
--import qualified TaskMachine.Config        as TM
--import qualified TaskMachine.UI.ListScreen as TM

{- Mockup UI

Purge | Refresh | Search _________
----------------------------------
  (A) do +stuff
x (B) and other +stuff



---------------------------------
Edit _____________________________

-}

-- Implementation plan:
-- [_] find/create suitable task list implementation
--  * keep original todo.txt order
--  * edit tasks, delete tasks, append tasks
--  * no reordering of tasks necessary
--  * sort by different metrics
--  * filter by different metrics
-- [_] load tasks from file specified in arguments
--  * report if file doesn't exist
--  * report if incorrect format (parse errors)
--  * warn if file only readable
-- [_] display loaded tasks in UI

drawUIState :: UIState -> [B.Widget RName]
drawUIState s =
  let wTopBar = const placeholderTopBar
      wTaskList = renderTaskList (taskList s) (taskEdit s)
      wNewTask = const placeholderNewTask
  in  pure $ case B.focusGetCurrent (focus s) of
        Nothing           -> B.vBox [wTopBar False, wTaskList False, wNewTask False] -- should never happen
        (Just BRTopBar)   -> B.vBox [wTopBar True,  wTaskList False, wNewTask False]
        (Just BRTaskList) -> B.vBox [wTopBar False, wTaskList True,  wNewTask False]
        (Just BRNewTask)  -> B.vBox [wTopBar False, wTaskList False, wNewTask True ]

updateUIState :: UIState -> B.BrickEvent RName () -> B.EventM RName (B.Next UIState)
updateUIState s e =
  case B.focusGetCurrent (focus s) of
    Nothing           -> undefined
    (Just BRTopBar)   -> placeholderUpdate s e
    (Just BRTaskList) -> updateTaskList s e
    (Just BRNewTask)  -> placeholderUpdate s e

placeholderUpdate :: UIState -> B.BrickEvent RName () -> B.EventM RName (B.Next UIState)
placeholderUpdate s (B.VtyEvent (VTY.EvKey VTY.KEsc [])) = B.halt s
placeholderUpdate s _                                    = B.continue s

myApp :: B.Theme -> B.App UIState () RName
myApp theme = B.App
  { B.appDraw         = drawUIState
  , B.appChooseCursor = B.showFirstCursor
  , B.appHandleEvent  = updateUIState
  , B.appStartEvent   = pure
  , B.appAttrMap      = const attrMap
  }
  where
    attrMap = B.themeToAttrMap theme

--  { uiConfig       :: TM.Config
--  , uiDBConnection :: DB.Connection
--  , uiScreenState  :: ScreenState
--  }
--
--data ScreenState
--  = Dummy
----  = ScreenList TM.ListScreen
--
--defaultTheme :: B.Theme
--defaultTheme = B.newTheme VTY.defAttr
--  [ ("taskList" <> "normal",    withStyle VTY.bold $ B.fg VTY.cyan)
--  , ("taskList" <> "highlight",                      B.bg VTY.cyan)
--  ]
--  where withStyle = flip VTY.withStyle
--
--myApp :: B.App () () ResourceName
--myApp = B.App
--  { B.appDraw         = \_ -> [myTestWidget]
--  , B.appHandleEvent  = B.resizeOrQuit
--  , B.appStartEvent   = \s -> return s
--  , B.appChooseCursor = B.neverShowCursor
--  , B.appAttrMap      = const $ B.themeToAttrMap defaultTheme
--  }
--  where
--    myTestWidget = B.withAttr ("taskList" <> "normal") (B.str "normal ") B.<+> B.withAttr ("taskList" <> "highlight") (B.str "style")

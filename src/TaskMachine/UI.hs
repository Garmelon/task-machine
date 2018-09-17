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

drawBaseLayer :: UIState -> B.Widget RName
drawBaseLayer s = B.vBox [placeholderTopBar, renderTaskList s, placeholderNewTask]

drawUIState :: UIState -> [B.Widget RName]
drawUIState s@UIState{errorPopup=Just p} = [renderPopup p, drawBaseLayer s]
drawUIState s                            = [drawBaseLayer s]

updateUIState :: UIState -> B.BrickEvent RName () -> B.EventM RName (B.Next UIState)
-- Closing any popups
updateUIState s@UIState{errorPopup=Just _} (B.VtyEvent (VTY.EvKey VTY.KEnter [])) = B.continue s{errorPopup=Nothing}
updateUIState s@UIState{errorPopup=Just _} (B.VtyEvent (VTY.EvKey VTY.KEsc   [])) = B.continue s{errorPopup=Nothing}
--updateUIState s@UIState{errorPopup=Just p} (B.VtyEvent e) = do
--  newPopup <- handlePopupEvent e p
--  B.continue s{errorPopup=Just newPopup}
-- If there's no password
updateUIState s e =
  case B.focusGetCurrent (focus s) of
    Nothing           -> B.halt s
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

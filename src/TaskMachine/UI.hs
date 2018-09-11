{-# LANGUAGE OverloadedStrings #-}

module TaskMachine.UI where

--import           Data.Monoid
--
import qualified Brick                as B
import qualified Brick.AttrMap        as B
import qualified Brick.Widgets.List   as B
import qualified Data.Vector          as V
import qualified Graphics.Vty         as VTY

import           TaskMachine.TaskList
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

data RName = RTaskList
  deriving (Eq, Ord)

data UIState = UIState
  { taskList       :: B.List RName LTask
  , invisibleTasks :: V.Vector LTask
  }

startUIState :: V.Vector LTask -> UIState
startUIState = undefined

startState :: UIState
startState = UIState (B.list RTaskList V.empty 1) V.empty

myApp :: B.App UIState () RName
myApp = B.App
  { B.appDraw         = const []
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = B.resizeOrQuit
  , B.appStartEvent   = pure
  , B.appAttrMap      = const $ B.attrMap VTY.defAttr []
  }

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

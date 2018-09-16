{-# LANGUAGE OverloadedStrings #-}

-- | A collection of types necessary for the UI.
--
-- These were put in a separate module to avoid an import cycle.

module TaskMachine.UI.Types
  ( RName(..)
  , BigRing(..)
  , SmallRing(..)
  , UIState(..)
  , startUIState
  , bigFocusNext, bigFocusPrev
  , smallFocusNext, smallFocusPrev
  , defaultTheme
  ) where

import qualified Brick.Focus        as B
import qualified Brick.Themes       as B
import qualified Brick.Widgets.List as B
import qualified Brick.Widgets.Edit as B
import qualified Data.Vector        as V
import qualified Graphics.Vty       as VTY

import           TaskMachine.LTask

-- | Resource names
data RName
  = RSearchEdit
  | RTaskList
  | RTaskEdit
  | RNewEdit
  deriving (Eq, Show, Ord)

data BigRing
  = BRTopBar
  | BRTaskList
  | BRNewTask
  deriving (Eq)

data SmallRing
  = SRPrune
  | SRReload
  | SRSearch
  deriving (Eq)

-- | The state of the program and UI
data UIState = UIState
  { focus          :: B.FocusRing BigRing
  -- ^ 'B.FocusRing' for tab navigation
  , focusTopBar    :: B.FocusRing SmallRing
  -- ^ 'B.FocusRing' for the top bar, for ← and → arrow key navigation

  -- TOP BAR
  , searchEdit     :: B.Editor String RName
  -- ^ Content of the search field

  -- TASK LIST
  , taskList       :: B.List RName LTask
  -- ^ List to display tasks
  , invisibleTasks :: V.Vector LTask
  -- ^ All tasks that aren't displayed in the taskList due to search filters
  , taskEdit       :: Maybe (B.Editor String RName)
  -- ^ Task currently being edited

  -- NEW TASK
  , newEdit        :: B.Editor String RName
  -- ^ "New task" text field at the bottom
  }

-- | Create a starting UI state
startUIState :: V.Vector LTask -> UIState
startUIState ltasks = UIState
  { focus          = B.focusRing [BRTaskList, BRNewTask, BRTopBar]
  , focusTopBar    = B.focusRing [SRPrune, SRReload, SRSearch]
  , searchEdit     = B.editor RSearchEdit (Just 1) ""
  , taskList       = B.list RTaskList ltasks 1
  , invisibleTasks = V.empty
  , taskEdit       = Nothing
  , newEdit        = B.editor RNewEdit (Just 1) ""
  }

bigFocusNext :: UIState -> UIState
bigFocusNext s = s{focus=B.focusNext (focus s)}

bigFocusPrev :: UIState -> UIState
bigFocusPrev s = s{focus=B.focusPrev (focus s)}

smallFocusNext :: UIState -> UIState
smallFocusNext s = s{focusTopBar=B.focusNext (focusTopBar s)}

smallFocusPrev :: UIState -> UIState
smallFocusPrev s = s{focusTopBar=B.focusPrev (focusTopBar s)}

defaultTheme :: B.Theme
defaultTheme = B.newTheme VTY.defAttr
  [ ("normal"   ,                                                            none)
  , ("normal"    <> "description",                                           none)
  , ("normal"    <> "priority",                      fg VTY.cyan   $ st' VTY.bold)
  , ("normal"    <> "priority" <> "A",               fg VTY.red    $ st' VTY.bold)
  , ("normal"    <> "priority" <> "B",               fg VTY.yellow $ st' VTY.bold)
  , ("normal"    <> "priority" <> "C",               fg VTY.green  $ st' VTY.bold)
  , ("highlight",                                                    bg' VTY.blue)
  , ("highlight" <> "description",                                   bg' VTY.blue)
  , ("highlight" <> "priority",        bg VTY.blue $ fg VTY.cyan   $ st' VTY.bold)
  , ("highlight" <> "priority" <> "A", bg VTY.blue $ fg VTY.red    $ st' VTY.bold)
  , ("highlight" <> "priority" <> "B", bg VTY.blue $ fg VTY.yellow $ st' VTY.bold)
  , ("highlight" <> "priority" <> "C", bg VTY.blue $ fg VTY.green  $ st' VTY.bold)
  ]
  where
    fg  = flip VTY.withForeColor
    bg  = flip VTY.withBackColor
    --st  = flip VTY.withStyle
    --fg' = VTY.withForeColor none
    bg' = VTY.withBackColor none
    st' = VTY.withStyle     none
    none = VTY.defAttr

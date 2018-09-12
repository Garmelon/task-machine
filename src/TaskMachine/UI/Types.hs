{-# LANGUAGE OverloadedStrings #-}

-- | A collection of types necessary for the UI.
--
-- These were put in a separate module to avoid an import cycle.

module TaskMachine.UI.Types
  ( RName(..)
  , UIState(..)
  , startUIState
  , defaultTheme
  ) where

import qualified Brick.Focus        as B
import qualified Brick.Themes       as B
import qualified Brick.Widgets.List as B
import qualified Data.Vector        as V
import qualified Graphics.Vty       as VTY

import           TaskMachine.LTask

-- | Resource names
data RName
  -- These can be tab-cycled through
  = RTopBar
  | RTaskList
  | REdit
  -- Items in the top bar that are selected with the ← and → arrow keys
  | RPrune
  | RReload
  | RSearch
  deriving (Eq, Show, Ord)

-- | The state of the program and UI
data UIState = UIState
  { focus          :: B.FocusRing RName
  -- ^ 'B.FocusRing' for tab navigation
  , topBarFocus    :: B.FocusRing RName
  -- ^ 'B.FocusRing' for the top bar, for ← and → arrow key navigation
  , taskList       :: B.List RName LTask
  -- ^ List to display tasks
  , invisibleTasks :: V.Vector LTask
  -- ^ All tasks that aren't displayed in the taskList due to search filters
  }

-- | Create a starting UI state
startUIState :: V.Vector LTask -> UIState
startUIState ltasks = UIState
  { focus          = B.focusRing [RTaskList, REdit, RTopBar]
  , topBarFocus    = B.focusRing [RPrune, RReload, RSearch]
  , taskList       = B.list RTaskList ltasks 1
  , invisibleTasks = V.empty
  }

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
    st  = flip VTY.withStyle
    fg' = VTY.withForeColor none
    bg' = VTY.withBackColor none
    st' = VTY.withStyle     none
    none = VTY.defAttr

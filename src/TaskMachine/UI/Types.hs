{-# LANGUAGE OverloadedStrings #-}

-- | A collection of types necessary for the UI.
--
-- These were put in a separate module to avoid an import cycle.

module TaskMachine.UI.Types
  ( RName(..)
  -- * UI state
  , UIState(..)
  , NewState
  , defaultTheme
  ) where

import qualified Brick                   as B
import qualified Brick.Themes            as B
import qualified Brick.Widgets.Dialog    as B
import qualified Brick.Widgets.Edit      as B
import qualified Brick.Widgets.List      as B
import qualified Graphics.Vty            as VTY

import           TaskMachine.Options
import           TaskMachine.UI.Popup
import           TaskMachine.UI.Task
import           TaskMachine.UI.TaskEdit
import           TaskMachine.UI.TaskList

-- | Resource names
data RName
  = RTaskList
  | RTaskEdit
  deriving (Eq, Show, Ord)

{- UI state -}

data UIState = UIState
  { options    :: Options -- includes todo file and other config
  , errorPopup :: Maybe (Popup RName (UIState -> NewState))
  , tasks      :: TaskList RName
  , editor     :: Maybe (TaskEdit RName)
  }

type NewState = B.EventM RName (B.Next UIState)

defaultTheme :: B.Theme
defaultTheme = B.newTheme VTY.defAttr
  [ (B.dialogAttr,                                    none)
  , (B.buttonAttr,                                    none)
  , (B.buttonSelectedAttr,                    bg' VTY.blue)
  , (B.editAttr,                                      none)
  , (B.editFocusedAttr,                       bg' VTY.blue)
  , (B.listAttr,                                      none)
  , (B.listSelectedAttr,                      st' VTY.bold)
  , (B.listSelectedFocusedAttr, bg VTY.blue $ st' VTY.bold)
  , (taskAttr,                                        none)
  , (taskCompletionAttr,                              none)
  , (taskPriorityAttr,        fg VTY.cyan   $ st' VTY.bold)
  , (taskPriorityAttr <> "A", fg VTY.red    $ st' VTY.bold)
  , (taskPriorityAttr <> "B", fg VTY.yellow $ st' VTY.bold)
  , (taskPriorityAttr <> "C", fg VTY.green  $ st' VTY.bold)
  , (taskDueAttr,                      fg' VTY.brightBlack)
  , (taskCreatedAttr,                  fg' VTY.brightBlack)
  , (taskProjectAttr,                       fg' VTY.yellow)
  , (taskContextAttr,                         fg' VTY.cyan)
  , (taskKeyValueAttr,                     fg' VTY.magenta)
  ]
  where
    fg  = flip VTY.withForeColor
    bg  = flip VTY.withBackColor
    --st  = flip VTY.withStyle
    fg' = VTY.withForeColor none
    bg' = VTY.withBackColor none
    st' = VTY.withStyle     none
    none = VTY.defAttr

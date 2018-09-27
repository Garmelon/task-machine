{-# LANGUAGE OverloadedStrings #-}

-- | A collection of types necessary for the UI.
--
-- These were put in a separate module to avoid an import cycle.

module TaskMachine.UI.Types
  ( RName(..)
  , BigRing(..)
  --, SmallRing(..)
  -- * Popups
  --, Popup
  --, popup
  --, renderPopup
  --, handlePopupEvent
  -- * UI state
  , UIState(..)
  , NewState
  , bigFocusNext, bigFocusPrev
  --, smallFocusNext, smallFocusPrev
  , defaultTheme
  ) where

import qualified Brick                   as B
import qualified Brick.Focus             as B
import qualified Brick.Themes            as B
import qualified Brick.Widgets.Dialog    as B
import qualified Brick.Widgets.Edit      as B
import qualified Brick.Widgets.List      as B
import qualified Graphics.Vty            as VTY
--import qualified Data.Vector          as V

--import           TaskMachine.LTask
import           TaskMachine.Options
import           TaskMachine.UI.Popup
import           TaskMachine.UI.Task
import           TaskMachine.UI.TaskList

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

{-
data SmallRing
  = SRPurge
  | SRReload
  | SRSearch
  deriving (Eq)
-}

{- Popup -}

{-
data Popup = Popup (B.Dialog ()) (B.Widget RName)

popup :: String -> String -> Popup
popup title content =
  let dialog = B.dialog (Just title) (Just (0,[("OK",())])) 70 -- with a min terminal width of 80
      widget = B.str content
  in  Popup dialog widget

renderPopup :: Popup -> B.Widget RName
renderPopup (Popup dialog widget) = B.renderDialog dialog widget

handlePopupEvent :: VTY.Event -> Popup -> B.EventM RName Popup
handlePopupEvent e (Popup dialog widget) = Popup <$> B.handleDialogEvent e dialog <*> pure widget
-}

{- UI state -}

data UIState = UIState
  { options    :: Options -- includes todo file and other config
  , focus      :: B.FocusRing BigRing -- focus on the top, middle or bottom part

  -- popups
  , errorPopup :: Maybe (PopupOk RName)

  -- tasks
  , tasks      :: TaskList RName
  }

type NewState = B.EventM RName (B.Next UIState)







{-
  , focus          :: B.FocusRing BigRing
  -- ^ 'B.FocusRing' for tab navigation
  --, focusTopBar    :: B.FocusRing SmallRing
  -- ^ 'B.FocusRing' for the top bar, for ← and → arrow key navigation
  , errorPopup     :: Maybe Popup

  -- TOP BAR
  --, searchEdit     :: B.Editor String RName
  -- ^ Content of the search field

  -- TASK LIST
  , taskList       :: B.List RName LTask
  -- ^ List to display tasks
  , invisibleTasks :: V.Vector LTask
  -- ^ All tasks that aren't displayed in the taskList due to search filters
  , taskEdit       :: Maybe (B.Editor String RName)
  -- ^ Task currently being edited

  -- NEW TASK
  --, newEdit        :: B.Editor String RName
  -- ^ "New task" text field at the bottom
  }

-- | Create a starting UI state
startUIState :: V.Vector LTask -> UIState
startUIState ltasks = UIState
  { focus          = B.focusRing [BRTaskList, BRNewTask, BRTopBar]
  --, focusTopBar    = B.focusRing [SRPrune, SRReload, SRSearch]
  , errorPopup     = Nothing
  --, searchEdit     = B.editor RSearchEdit (Just 1) ""
  , taskList       = B.list RTaskList ltasks 1
  , invisibleTasks = V.empty
  , taskEdit       = Nothing
  --, newEdit        = B.editor RNewEdit (Just 1) ""
  }
-}

bigFocusNext :: UIState -> UIState
bigFocusNext s = s{focus=B.focusNext (focus s)}

bigFocusPrev :: UIState -> UIState
bigFocusPrev s = s{focus=B.focusPrev (focus s)}

{-
smallFocusNext :: UIState -> UIState
smallFocusNext s = s{focusTopBar=B.focusNext (focusTopBar s)}

smallFocusPrev :: UIState -> UIState
smallFocusPrev s = s{focusTopBar=B.focusPrev (focusTopBar s)}
-}

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

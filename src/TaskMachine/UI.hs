module TaskMachine.UI
  ( myApp
  , startUIState
  , loadTasks
  ) where

import qualified Brick                     as B
import qualified Brick.Focus               as B
import qualified Brick.Themes              as B
import qualified Data.Vector               as V
import qualified Graphics.Vty.Input.Events as VTY

import           TaskMachine.LTask
import           TaskMachine.Options
import           TaskMachine.UI.Popup
import           TaskMachine.UI.TaskList
import           TaskMachine.UI.Types

{- Rendering -}

placeholderTopBar :: B.Widget n
placeholderTopBar = B.str "Prune | Reload | Search: " B.<+> B.vLimit 1 (B.fill '_')

placeholderNewTask :: B.Widget RName
placeholderNewTask = B.str "New: " B.<+> B.vLimit 1 (B.fill '_')

drawBaseLayer :: UIState -> B.Widget RName
drawBaseLayer s = B.vBox [placeholderTopBar, renderTaskList True (tasks s), placeholderNewTask]

drawUIState :: UIState -> [B.Widget RName]
drawUIState s@UIState{errorPopup=Just p} = [renderPopupOk p, drawBaseLayer s]
drawUIState s                            = [drawBaseLayer s]

{- Actions -}

loadTasks :: UIState -> IO UIState
loadTasks s = do
  let file = oTodofile $ options s
  result <- loadLTasks file
  case result of
    -- TODO: Improve error handling when loading files
    Left errorMessage -> pure s{errorPopup=Just $ popupOk "Error loading tasks" errorMessage}
    Right ltasks      -> pure s{tasks=taskList RTaskList ltasks}

{- Updating the state -}

rootBehavior :: UIState -> VTY.Event -> NewState
rootBehavior s _ = B.continue s

closeBehavior :: (UIState -> VTY.Event -> NewState) -> UIState -> VTY.Event -> NewState
closeBehavior _ s (VTY.EvKey VTY.KEsc        []) = B.halt s
closeBehavior _ s (VTY.EvKey (VTY.KChar 'q') []) = B.halt s
closeBehavior f s e                              = f s e -- wrapper around another behavior

{-
focusBehavior :: (UIState -> VTY.Event -> Result) -> UIState -> VTY.Event -> Result
focusBehavior _ s (VTY.EvKey (VTY.KChar '\t') []) = B.continue $ bigFocusNext s
focusBehavior _ s (VTY.EvKey VTY.KBackTab     []) = B.continue $ bigFocusPrev s
focusBehavior f s e = f s e -- wrapper around another behavior
-}

selectBehavior :: UIState -> VTY.Event -> NewState
selectBehavior s@UIState{errorPopup=Just popup} e = undefined popup s e
selectBehavior s e = closeBehavior rootBehavior s e

updateUIState :: UIState -> B.BrickEvent RName () -> NewState
updateUIState s (B.VtyEvent e) = selectBehavior s e
updateUIState s _              = B.continue s

{-
updateUIState :: UIState -> B.BrickEvent RName () -> B.EventM RName (B.Next UIState)
-- Closing error popup
updateUIState s@UIState{errorPopup=Just _} (B.VtyEvent (VTY.EvKey VTY.KEnter [])) = B.continue s{errorPopup=Nothing}
updateUIState s@UIState{errorPopup=Just _} (B.VtyEvent (VTY.EvKey VTY.KEsc   [])) = B.continue s{errorPopup=Nothing}
--updateUIState s@UIState{errorPopup=Just p} (B.VtyEvent e) = do
--  newPopup <- handlePopupEvent e p
--  B.continue s{errorPopup=Just newPopup}
updateUIState s e =
  case B.focusGetCurrent (focus s) of
    Nothing           -> B.halt s
    (Just BRTopBar)   -> placeholderUpdate s e
    --(Just BRTaskList) -> updateTaskList s e
    (Just BRTaskList) -> placeholderUpdate s e
    (Just BRNewTask)  -> placeholderUpdate s e

placeholderUpdate :: UIState -> B.BrickEvent RName () -> B.EventM RName (B.Next UIState)
placeholderUpdate s (B.VtyEvent (VTY.EvKey VTY.KEsc         [])) = B.halt s
placeholderUpdate s (B.VtyEvent (VTY.EvKey (VTY.KChar '\t') [])) = B.continue $ bigFocusNext s
placeholderUpdate s (B.VtyEvent (VTY.EvKey VTY.KBackTab     [])) = B.continue $ bigFocusPrev s
placeholderUpdate s _                                            = B.continue s
-}

{- Starting the app -}

myApp :: B.App UIState () RName
myApp = B.App
  { B.appDraw         = drawUIState
  , B.appChooseCursor = B.showFirstCursor
  , B.appHandleEvent  = updateUIState
  , B.appStartEvent   = pure
  , B.appAttrMap      = const (B.themeToAttrMap defaultTheme)
  }

startUIState :: Options -> UIState
startUIState o = UIState
  { options        = o
  , focus          = B.focusRing [BRTaskList, BRNewTask, BRTopBar]
  , errorPopup     = Nothing
  , tasks          = taskList RTaskList V.empty
  }

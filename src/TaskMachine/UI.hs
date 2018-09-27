module TaskMachine.UI
  ( myApp
  , startUIState
  ) where

import qualified Brick                     as B
import qualified Brick.Focus               as B
import qualified Brick.Themes              as B
import qualified Data.Vector               as V
import qualified Graphics.Vty.Input.Events as VTY

import           TaskMachine.Options
import           TaskMachine.UI.NewTask
import           TaskMachine.UI.Popup
import           TaskMachine.UI.TaskList
import           TaskMachine.UI.TopBar
import           TaskMachine.UI.Types

drawBaseLayer :: UIState -> B.Widget RName
drawBaseLayer s = B.vBox [placeholderTopBar, renderTaskList s, placeholderNewTask]

drawUIState :: UIState -> [B.Widget RName]
drawUIState s@UIState{errorPopup=Just p} = [renderPopupOk p, drawBaseLayer s]
drawUIState s                            = [drawBaseLayer s]

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
  , taskList       = newTaskList V.empty
  , invisibleTasks = V.empty
  }

module TaskMachine.UI
  ( myApp
  , startUIState
  ) where

import qualified Brick                     as B
import qualified Brick.Themes              as B
import qualified Data.Vector               as V
import qualified Graphics.Vty.Input.Events as VTY

import           TaskMachine.Options
import           TaskMachine.UI.Behaviors
import           TaskMachine.UI.Popup
import           TaskMachine.UI.TaskList
import           TaskMachine.UI.Types

{- Rendering -}

drawTaskList :: UIState -> B.Widget RName
drawTaskList s = renderTaskList (editor s) True (tasks s)

drawUIState :: UIState -> [B.Widget RName]
drawUIState s@UIState{errorPopup=Just p} = [renderPopup p, drawTaskList s]
drawUIState s                            = [drawTaskList s]

{- Updating the state -}

selectBehavior :: UIState -> VTY.Event -> NewState
-- Deal with popup if there is one
selectBehavior s@UIState{errorPopup=Just p} e = closeModifier (popupBehavior p) s e
-- Continue editing task if previously editing a task
selectBehavior s@UIState{editor=Just edit} e = taskEditBehavior edit s e
-- Default task list behavior
selectBehavior s e = closeModifier taskListBehavior s e

updateUIState :: UIState -> B.BrickEvent RName () -> NewState
updateUIState s (B.VtyEvent e) = selectBehavior s e
updateUIState s _              = B.continue s

{- Starting the app -}

myApp :: B.App UIState () RName
myApp = B.App
  { B.appDraw         = drawUIState
  , B.appChooseCursor = B.showFirstCursor
  , B.appHandleEvent  = updateUIState
  , B.appStartEvent   = actionLoad
  , B.appAttrMap      = const (B.themeToAttrMap defaultTheme)
  }

startUIState :: Options -> UIState
startUIState o = UIState
  { options    = o
  , errorPopup = Nothing
  , tasks      = taskList RTaskList V.empty
  , editor   = Nothing
  }

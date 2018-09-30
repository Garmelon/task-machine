module TaskMachine.UI
  ( myApp
  , startUIState
  ) where

import qualified Brick                     as B
import qualified Brick.Themes              as B
import           Control.Monad.Trans
import qualified Data.Vector               as V
import qualified Graphics.Vty.Input.Events as VTY

import           TaskMachine.Options
import           TaskMachine.UI.Behaviors
import           TaskMachine.UI.Popup
import           TaskMachine.UI.Stuff
import           TaskMachine.UI.TaskList
import           TaskMachine.UI.Types

{- Rendering -}

drawTaskList :: UIState -> B.Widget RName
drawTaskList s = renderTaskList (taskEdit s) True (tasks s)

drawUIState :: UIState -> [B.Widget RName]
drawUIState s@UIState{errorPopup=Just p} = [renderPopup p, drawTaskList s]
drawUIState s                            = [drawTaskList s]

{- Updating the state -}

closeBehavior :: (UIState -> VTY.Event -> NewState) -> UIState -> VTY.Event -> NewState
closeBehavior _ s (VTY.EvKey VTY.KEsc        []) = B.halt s
closeBehavior _ s (VTY.EvKey (VTY.KChar 'q') []) = B.halt s
closeBehavior f s e                              = f s e -- wrapper around another behavior

popupBehavior :: Popup RName (UIState -> NewState) -> UIState -> VTY.Event -> NewState
popupBehavior p s (VTY.EvKey VTY.KEnter []) =
  case popupSelection p of
    Nothing     -> B.continue s{errorPopup=Nothing}
    Just action -> action s{errorPopup=Nothing}
popupBehavior p s e = do
  newPopup <- handlePopupEvent e p
  B.continue s{errorPopup=Just newPopup}

selectBehavior :: UIState -> VTY.Event -> NewState
-- Deal with popup if there is one
selectBehavior s@UIState{errorPopup=Just p} e = popupBehavior p s e
-- Under the assumption that tasks can only be edited while the task list is focused, edit a task
selectBehavior s@UIState{taskEdit=Just edit} e = taskEditBehavior edit s e
-- If nothing immediately jumps out at you, see which part has focus.
selectBehavior s e = closeBehavior taskListBehavior s e

updateUIState :: UIState -> B.BrickEvent RName () -> NewState
updateUIState s (B.VtyEvent e) = selectBehavior s e
updateUIState s _              = B.continue s

{- Starting the app -}

startEvent :: UIState -> B.EventM RName UIState
startEvent = liftIO . loadTasks

myApp :: B.App UIState () RName
myApp = B.App
  { B.appDraw         = drawUIState
  , B.appChooseCursor = B.showFirstCursor
  , B.appHandleEvent  = updateUIState
  , B.appStartEvent   = startEvent
  , B.appAttrMap      = const (B.themeToAttrMap defaultTheme)
  }

startUIState :: Options -> UIState
startUIState o = UIState
  { options    = o
  , errorPopup = Nothing
  , tasks      = taskList RTaskList V.empty
  , taskEdit   = Nothing
  }
